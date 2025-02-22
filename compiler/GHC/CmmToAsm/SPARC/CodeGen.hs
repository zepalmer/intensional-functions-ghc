{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Generating machine code (instruction selection)
--
-- (c) The University of Glasgow 1996-2013
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module GHC.CmmToAsm.SPARC.CodeGen (
        cmmTopCodeGen,
        generateJumpTableForInstr,
        InstrBlock
)

where

#include "HsVersions.h"

-- NCG stuff:
import GHC.Prelude

import GHC.CmmToAsm.SPARC.Base
import GHC.CmmToAsm.SPARC.CodeGen.Sanity
import GHC.CmmToAsm.SPARC.CodeGen.Amode
import GHC.CmmToAsm.SPARC.CodeGen.CondCode
import GHC.CmmToAsm.SPARC.CodeGen.Gen64
import GHC.CmmToAsm.SPARC.CodeGen.Gen32
import GHC.CmmToAsm.SPARC.CodeGen.Base
import GHC.CmmToAsm.SPARC.Instr
import GHC.CmmToAsm.SPARC.Imm
import GHC.CmmToAsm.SPARC.AddrMode
import GHC.CmmToAsm.SPARC.Regs
import GHC.CmmToAsm.SPARC.Stack
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.Format
import GHC.CmmToAsm.Monad   ( NatM, getNewRegNat, getNewLabelNat, getPlatform, getConfig )
import GHC.CmmToAsm.Config

-- Our intermediate code:
import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.CmmToAsm.PIC
import GHC.Platform.Reg
import GHC.Cmm.CLabel
import GHC.CmmToAsm.CPrim

-- The rest:
import GHC.Types.Basic
import GHC.Data.FastString
import GHC.Data.OrdList
import GHC.Utils.Panic
import GHC.Platform

import Control.Monad    ( mapAndUnzipM )

-- | Top level code generation
cmmTopCodeGen :: RawCmmDecl
              -> NatM [NatCmmDecl RawCmmStatics Instr]

cmmTopCodeGen (CmmProc info lab live graph)
 = do let blocks = toBlockListEntryFirst graph
      (nat_blocks,statics) <- mapAndUnzipM basicBlockCodeGen blocks

      let proc = CmmProc info lab live (ListGraph $ concat nat_blocks)
      let tops = proc : concat statics

      return tops

cmmTopCodeGen (CmmData sec dat) =
  return [CmmData sec dat]  -- no translation, we just use CmmStatic


-- | Do code generation on a single block of CMM code.
--      code generation may introduce new basic block boundaries, which
--      are indicated by the NEWBLOCK instruction.  We must split up the
--      instruction stream into basic blocks again.  Also, we extract
--      LDATAs here too.
basicBlockCodeGen :: CmmBlock
                  -> NatM ( [NatBasicBlock Instr]
                          , [NatCmmDecl RawCmmStatics Instr])

basicBlockCodeGen block = do
  let (_, nodes, tail)  = blockSplit block
      id = entryLabel block
      stmts = blockToList nodes
  platform <- getPlatform
  mid_instrs <- stmtsToInstrs stmts
  tail_instrs <- stmtToInstrs tail
  let instrs = mid_instrs `appOL` tail_instrs
  let
        (top,other_blocks,statics)
                = foldrOL mkBlocks ([],[],[]) instrs

        mkBlocks (NEWBLOCK id) (instrs,blocks,statics)
          = ([], BasicBlock id instrs : blocks, statics)

        mkBlocks (LDATA sec dat) (instrs,blocks,statics)
          = (instrs, blocks, CmmData sec dat:statics)

        mkBlocks instr (instrs,blocks,statics)
          = (instr:instrs, blocks, statics)

        -- do intra-block sanity checking
        blocksChecked
                = map (checkBlock platform block)
                $ BasicBlock id top : other_blocks

  return (blocksChecked, statics)


-- | Convert some Cmm statements to SPARC instructions.
stmtsToInstrs :: [CmmNode e x] -> NatM InstrBlock
stmtsToInstrs stmts
   = do instrss <- mapM stmtToInstrs stmts
        return (concatOL instrss)


stmtToInstrs :: CmmNode e x -> NatM InstrBlock
stmtToInstrs stmt = do
  platform <- getPlatform
  config <- getConfig
  case stmt of
    CmmComment s   -> return (unitOL (COMMENT s))
    CmmTick {}     -> return nilOL
    CmmUnwind {}   -> return nilOL

    CmmAssign reg src
      | isFloatType ty  -> assignReg_FltCode format reg src
      | isWord64 ty     -> assignReg_I64Code        reg src
      | otherwise       -> assignReg_IntCode format reg src
        where ty = cmmRegType platform reg
              format = cmmTypeFormat ty

    CmmStore addr src _
      | isFloatType ty  -> assignMem_FltCode format addr src
      | isWord64 ty     -> assignMem_I64Code      addr src
      | otherwise       -> assignMem_IntCode format addr src
        where ty = cmmExprType platform src
              format = cmmTypeFormat ty

    CmmUnsafeForeignCall target result_regs args
       -> genCCall target result_regs args

    CmmBranch   id              -> genBranch id
    CmmCondBranch arg true false _ -> do
      b1 <- genCondJump true arg
      b2 <- genBranch false
      return (b1 `appOL` b2)
    CmmSwitch arg ids   -> genSwitch config arg ids
    CmmCall { cml_target = arg } -> genJump arg

    _
     -> panic "stmtToInstrs: statement should have been cps'd away"


{-
Now, given a tree (the argument to a CmmLoad) that references memory,
produce a suitable addressing mode.

A Rule of the Game (tm) for Amodes: use of the addr bit must
immediately follow use of the code part, since the code part puts
values in registers which the addr then refers to.  So you can't put
anything in between, lest it overwrite some of those registers.  If
you need to do some other computation between the code part and use of
the addr bit, first store the effective address from the amode in a
temporary, then do the other computation, and then use the temporary:

    code
    LEA amode, tmp
    ... other computation ...
    ... (tmp) ...
-}



-- | Convert a BlockId to some CmmStatic data
jumpTableEntry :: Platform -> Maybe BlockId -> CmmStatic
jumpTableEntry platform Nothing = CmmStaticLit (CmmInt 0 (wordWidth platform))
jumpTableEntry _ (Just blockid) = CmmStaticLit (CmmLabel blockLabel)
    where blockLabel = blockLbl blockid



-- -----------------------------------------------------------------------------
-- Generating assignments

-- Assignments are really at the heart of the whole code generation
-- business.  Almost all top-level nodes of any real importance are
-- assignments, which correspond to loads, stores, or register
-- transfers.  If we're really lucky, some of the register transfers
-- will go away, because we can use the destination register to
-- complete the code generation for the right hand side.  This only
-- fails when the right hand side is forced into a fixed register
-- (e.g. the result of a call).

assignMem_IntCode :: Format -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_IntCode pk addr src = do
    (srcReg, code) <- getSomeReg src
    Amode dstAddr addr_code <- getAmode addr
    return $ code `appOL` addr_code `snocOL` ST pk srcReg dstAddr


assignReg_IntCode :: Format -> CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_IntCode _ reg src = do
    platform <- getPlatform
    r <- getRegister src
    let dst = getRegisterReg platform reg
    return $ case r of
        Any _ code         -> code dst
        Fixed _ freg fcode -> fcode `snocOL` OR False g0 (RIReg freg) dst



-- Floating point assignment to memory
assignMem_FltCode :: Format -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignMem_FltCode pk addr src = do
    platform <- getPlatform
    Amode dst__2 code1 <- getAmode addr
    (src__2, code2) <- getSomeReg src
    tmp1 <- getNewRegNat pk
    let
        pk__2   = cmmExprType platform src
        code__2 = code1 `appOL` code2 `appOL`
            if   formatToWidth pk == typeWidth pk__2
            then unitOL (ST pk src__2 dst__2)
            else toOL   [ FxTOy (cmmTypeFormat pk__2) pk src__2 tmp1
                        , ST    pk tmp1 dst__2]
    return code__2

-- Floating point assignment to a register/temporary
assignReg_FltCode :: Format -> CmmReg  -> CmmExpr -> NatM InstrBlock
assignReg_FltCode pk dstCmmReg srcCmmExpr = do
    platform <- getPlatform
    srcRegister <- getRegister srcCmmExpr
    let dstReg  = getRegisterReg platform dstCmmReg

    return $ case srcRegister of
        Any _ code                  -> code dstReg
        Fixed _ srcFixedReg srcCode -> srcCode `snocOL` FMOV pk srcFixedReg dstReg




genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock

genJump (CmmLit (CmmLabel lbl))
  = return (toOL [CALL (Left target) 0 True, NOP])
  where
    target = ImmCLbl lbl

genJump tree
  = do
        (target, code) <- getSomeReg tree
        return (code `snocOL` JMP (AddrRegReg target g0)  `snocOL` NOP)

-- -----------------------------------------------------------------------------
--  Unconditional branches

genBranch :: BlockId -> NatM InstrBlock
genBranch = return . toOL . mkJumpInstr


-- -----------------------------------------------------------------------------
--  Conditional jumps

{-
Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

SPARC: First, we have to ensure that the condition codes are set
according to the supplied comparison operation.  We generate slightly
different code for floating point comparisons, because a floating
point operation cannot directly precede a @BF@.  We assume the worst
and fill that slot with a @NOP@.

SPARC: Do not fill the delay slots here; you will confuse the register
allocator.
-}


genCondJump
    :: BlockId      -- the branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock



genCondJump bid bool = do
  CondCode is_float cond code <- getCondCode bool
  return (
       code `appOL`
       toOL (
         if   is_float
         then [NOP, BF cond False bid, NOP]
         else [BI cond False bid, NOP]
       )
    )



-- -----------------------------------------------------------------------------
-- Generating a table-branch

genSwitch :: NCGConfig -> CmmExpr -> SwitchTargets -> NatM InstrBlock
genSwitch config expr targets
        | ncgPIC config
        = error "MachCodeGen: sparc genSwitch PIC not finished\n"

        | otherwise
        = do    (e_reg, e_code) <- getSomeReg (cmmOffset (ncgPlatform config) expr offset)

                base_reg        <- getNewRegNat II32
                offset_reg      <- getNewRegNat II32
                dst             <- getNewRegNat II32

                label           <- getNewLabelNat

                return $ e_code `appOL`
                 toOL
                        [ -- load base of jump table
                          SETHI (HI (ImmCLbl label)) base_reg
                        , OR    False base_reg (RIImm $ LO $ ImmCLbl label) base_reg

                        -- the addrs in the table are 32 bits wide..
                        , SLL   e_reg (RIImm $ ImmInt 2) offset_reg

                        -- load and jump to the destination
                        , LD      II32 (AddrRegReg base_reg offset_reg) dst
                        , JMP_TBL (AddrRegImm dst (ImmInt 0)) ids label
                        , NOP ]
  where (offset, ids) = switchTargetsToTable targets

generateJumpTableForInstr :: Platform -> Instr
                          -> Maybe (NatCmmDecl RawCmmStatics Instr)
generateJumpTableForInstr platform (JMP_TBL _ ids label) =
  let jumpTable = map (jumpTableEntry platform) ids
  in Just (CmmData (Section ReadOnlyData label) (CmmStaticsRaw label jumpTable))
generateJumpTableForInstr _ _ = Nothing



-- -----------------------------------------------------------------------------
-- Generating C calls

{-
   Now the biggest nightmare---calls.  Most of the nastiness is buried in
   @get_arg@, which moves the arguments to the correct registers/stack
   locations.  Apart from that, the code is easy.

   The SPARC calling convention is an absolute
   nightmare.  The first 6x32 bits of arguments are mapped into
   %o0 through %o5, and the remaining arguments are dumped to the
   stack, beginning at [%sp+92].  (Note that %o6 == %sp.)

   If we have to put args on the stack, move %o6==%sp down by
   the number of words to go on the stack, to ensure there's enough space.

   According to Fraser and Hanson's lcc book, page 478, fig 17.2,
   16 words above the stack pointer is a word for the address of
   a structure return value.  I use this as a temporary location
   for moving values from float to int regs.  Certainly it isn't
   safe to put anything in the 16 words starting at %sp, since
   this area can get trashed at any time due to window overflows
   caused by signal handlers.

   A final complication (if the above isn't enough) is that
   we can't blithely calculate the arguments one by one into
   %o0 .. %o5.  Consider the following nested calls:

       fff a (fff b c)

   Naive code moves a into %o0, and (fff b c) into %o1.  Unfortunately
   the inner call will itself use %o0, which trashes the value put there
   in preparation for the outer call.  Upshot: we need to calculate the
   args into temporary regs, and move those to arg regs or onto the
   stack only immediately prior to the call proper.  Sigh.
-}

genCCall
    :: ForeignTarget            -- function to call
    -> [CmmFormal]        -- where to put the result
    -> [CmmActual]        -- arguments (of mixed type)
    -> NatM InstrBlock



-- On SPARC under TSO (Total Store Ordering), writes earlier in the instruction stream
-- are guaranteed to take place before writes afterwards (unlike on PowerPC).
-- Ref: Section 8.4 of the SPARC V9 Architecture manual.
--
-- In the SPARC case we don't need a barrier.
--
genCCall (PrimTarget MO_ReadBarrier) _ _
 = return $ nilOL
genCCall (PrimTarget MO_WriteBarrier) _ _
 = return $ nilOL

genCCall (PrimTarget (MO_Prefetch_Data _)) _ _
 = return $ nilOL

genCCall target dest_regs args
 = do   -- work out the arguments, and assign them to integer regs
        argcode_and_vregs       <- mapM arg_to_int_vregs args
        let (argcodes, vregss)  = unzip argcode_and_vregs
        let vregs               = concat vregss

        let n_argRegs           = length allArgRegs
        let n_argRegs_used      = min (length vregs) n_argRegs


        -- deal with static vs dynamic call targets
        callinsns <- case target of
                ForeignTarget (CmmLit (CmmLabel lbl)) _ ->
                        return (unitOL (CALL (Left (litToImm (CmmLabel lbl))) n_argRegs_used False))

                ForeignTarget expr _
                 -> do  (dyn_c, dyn_rs) <- arg_to_int_vregs expr
                        let dyn_r = case dyn_rs of
                                      [dyn_r'] -> dyn_r'
                                      _ -> panic "SPARC.CodeGen.genCCall: arg_to_int"
                        return (dyn_c `snocOL` CALL (Right dyn_r) n_argRegs_used False)

                PrimTarget mop
                 -> do  res     <- outOfLineMachOp mop
                        case res of
                                Left lbl ->
                                        return (unitOL (CALL (Left (litToImm (CmmLabel lbl))) n_argRegs_used False))

                                Right mopExpr -> do
                                        (dyn_c, dyn_rs) <- arg_to_int_vregs mopExpr
                                        let dyn_r = case dyn_rs of
                                                      [dyn_r'] -> dyn_r'
                                                      _ -> panic "SPARC.CodeGen.genCCall: arg_to_int"
                                        return (dyn_c `snocOL` CALL (Right dyn_r) n_argRegs_used False)

        let argcode = concatOL argcodes

        let (move_sp_down, move_sp_up)
                   = let diff = length vregs - n_argRegs
                         nn   = if odd diff then diff + 1 else diff -- keep 8-byte alignment
                     in  if   nn <= 0
                         then (nilOL, nilOL)
                         else (unitOL (moveSp (-1*nn)), unitOL (moveSp (1*nn)))

        let transfer_code
                = toOL (move_final vregs allArgRegs extraStackArgsHere)

        platform <- getPlatform
        return
         $      argcode                 `appOL`
                move_sp_down            `appOL`
                transfer_code           `appOL`
                callinsns               `appOL`
                unitOL NOP              `appOL`
                move_sp_up              `appOL`
                assign_code platform dest_regs


-- | Generate code to calculate an argument, and move it into one
--      or two integer vregs.
arg_to_int_vregs :: CmmExpr -> NatM (OrdList Instr, [Reg])
arg_to_int_vregs arg = do platform <- getPlatform
                          arg_to_int_vregs' platform arg

arg_to_int_vregs' :: Platform -> CmmExpr -> NatM (OrdList Instr, [Reg])
arg_to_int_vregs' platform arg

        -- If the expr produces a 64 bit int, then we can just use iselExpr64
        | isWord64 (cmmExprType platform arg)
        = do    (ChildCode64 code r_lo) <- iselExpr64 arg
                let r_hi                = getHiVRegFromLo r_lo
                return (code, [r_hi, r_lo])

        | otherwise
        = do    (src, code)     <- getSomeReg arg
                let pk          = cmmExprType platform arg

                case cmmTypeFormat pk of

                 -- Load a 64 bit float return value into two integer regs.
                 FF64 -> do
                        v1 <- getNewRegNat II32
                        v2 <- getNewRegNat II32

                        let code2 =
                                code                            `snocOL`
                                FMOV FF64 src f0                `snocOL`
                                ST   FF32  f0 (spRel 16)        `snocOL`
                                LD   II32  (spRel 16) v1        `snocOL`
                                ST   FF32  f1 (spRel 16)        `snocOL`
                                LD   II32  (spRel 16) v2

                        return  (code2, [v1,v2])

                 -- Load a 32 bit float return value into an integer reg
                 FF32 -> do
                        v1 <- getNewRegNat II32

                        let code2 =
                                code                            `snocOL`
                                ST   FF32  src (spRel 16)       `snocOL`
                                LD   II32  (spRel 16) v1

                        return (code2, [v1])

                 -- Move an integer return value into its destination reg.
                 _ -> do
                        v1 <- getNewRegNat II32

                        let code2 =
                                code                            `snocOL`
                                OR False g0 (RIReg src) v1

                        return (code2, [v1])


-- | Move args from the integer vregs into which they have been
--      marshalled, into %o0 .. %o5, and the rest onto the stack.
--
move_final :: [Reg] -> [Reg] -> Int -> [Instr]

-- all args done
move_final [] _ _
        = []

-- out of aregs; move to stack
move_final (v:vs) [] offset
        = ST II32 v (spRel offset)
        : move_final vs [] (offset+1)

-- move into an arg (%o[0..5]) reg
move_final (v:vs) (a:az) offset
        = OR False g0 (RIReg v) a
        : move_final vs az offset


-- | Assign results returned from the call into their
--      destination regs.
--
assign_code :: Platform -> [LocalReg] -> OrdList Instr

assign_code _ [] = nilOL

assign_code platform [dest]
 = let  rep     = localRegType dest
        width   = typeWidth rep
        r_dest  = getRegisterReg platform (CmmLocal dest)

        result
                | isFloatType rep
                , W32   <- width
                = unitOL $ FMOV FF32 (regSingle $ fReg 0) r_dest

                | isFloatType rep
                , W64   <- width
                = unitOL $ FMOV FF64 (regSingle $ fReg 0) r_dest

                | not $ isFloatType rep
                , W32   <- width
                = unitOL $ mkRegRegMoveInstr platform (regSingle $ oReg 0) r_dest

                | not $ isFloatType rep
                , W64           <- width
                , r_dest_hi     <- getHiVRegFromLo r_dest
                = toOL  [ mkRegRegMoveInstr platform (regSingle $ oReg 0) r_dest_hi
                        , mkRegRegMoveInstr platform (regSingle $ oReg 1) r_dest]

                | otherwise
                = panic "SPARC.CodeGen.GenCCall: no match"

   in   result

assign_code _ _
        = panic "SPARC.CodeGen.GenCCall: no match"



-- | Generate a call to implement an out-of-line floating point operation
outOfLineMachOp
        :: CallishMachOp
        -> NatM (Either CLabel CmmExpr)

outOfLineMachOp mop
 = do   let functionName
                = outOfLineMachOp_table mop

        config  <- getConfig
        mopExpr <- cmmMakeDynamicReference config CallReference
                $  mkForeignLabel functionName Nothing ForeignLabelInExternalPackage IsFunction

        let mopLabelOrExpr
                = case mopExpr of
                        CmmLit (CmmLabel lbl)   -> Left lbl
                        _                       -> Right mopExpr

        return mopLabelOrExpr


-- | Decide what C function to use to implement a CallishMachOp
--
outOfLineMachOp_table
        :: CallishMachOp
        -> FastString

outOfLineMachOp_table mop
 = case mop of
        MO_F32_Exp    -> fsLit "expf"
        MO_F32_ExpM1  -> fsLit "expm1f"
        MO_F32_Log    -> fsLit "logf"
        MO_F32_Log1P  -> fsLit "log1pf"
        MO_F32_Sqrt   -> fsLit "sqrtf"
        MO_F32_Fabs   -> unsupported
        MO_F32_Pwr    -> fsLit "powf"

        MO_F32_Sin    -> fsLit "sinf"
        MO_F32_Cos    -> fsLit "cosf"
        MO_F32_Tan    -> fsLit "tanf"

        MO_F32_Asin   -> fsLit "asinf"
        MO_F32_Acos   -> fsLit "acosf"
        MO_F32_Atan   -> fsLit "atanf"

        MO_F32_Sinh   -> fsLit "sinhf"
        MO_F32_Cosh   -> fsLit "coshf"
        MO_F32_Tanh   -> fsLit "tanhf"

        MO_F32_Asinh  -> fsLit "asinhf"
        MO_F32_Acosh  -> fsLit "acoshf"
        MO_F32_Atanh  -> fsLit "atanhf"

        MO_F64_Exp    -> fsLit "exp"
        MO_F64_ExpM1  -> fsLit "expm1"
        MO_F64_Log    -> fsLit "log"
        MO_F64_Log1P  -> fsLit "log1p"
        MO_F64_Sqrt   -> fsLit "sqrt"
        MO_F64_Fabs   -> unsupported
        MO_F64_Pwr    -> fsLit "pow"

        MO_F64_Sin    -> fsLit "sin"
        MO_F64_Cos    -> fsLit "cos"
        MO_F64_Tan    -> fsLit "tan"

        MO_F64_Asin   -> fsLit "asin"
        MO_F64_Acos   -> fsLit "acos"
        MO_F64_Atan   -> fsLit "atan"

        MO_F64_Sinh   -> fsLit "sinh"
        MO_F64_Cosh   -> fsLit "cosh"
        MO_F64_Tanh   -> fsLit "tanh"

        MO_F64_Asinh  -> fsLit "asinh"
        MO_F64_Acosh  -> fsLit "acosh"
        MO_F64_Atanh  -> fsLit "atanh"

        MO_I64_ToI   -> fsLit "hs_int64ToInt"
        MO_I64_FromI -> fsLit "hs_intToInt64"
        MO_W64_ToW   -> fsLit "hs_word64ToWord"
        MO_W64_FromW -> fsLit "hs_wordToWord64"
        MO_x64_Neg   -> fsLit "hs_neg64"
        MO_x64_Add   -> fsLit "hs_add64"
        MO_x64_Sub   -> fsLit "hs_sub64"
        MO_x64_Mul   -> fsLit "hs_mul64"
        MO_I64_Quot  -> fsLit "hs_quotInt64"
        MO_I64_Rem   -> fsLit "hs_remInt64"
        MO_W64_Quot  -> fsLit "hs_quotWord64"
        MO_W64_Rem   -> fsLit "hs_remWord64"
        MO_x64_And   -> fsLit "hs_and64"
        MO_x64_Or    -> fsLit "hs_or64"
        MO_x64_Xor   -> fsLit "hs_xor64"
        MO_x64_Not   -> fsLit "hs_not64"
        MO_x64_Shl   -> fsLit "hs_uncheckedShiftL64"
        MO_I64_Shr   -> fsLit "hs_uncheckedIShiftRA64"
        MO_W64_Shr   -> fsLit "hs_uncheckedShiftRL64"
        MO_x64_Eq    -> fsLit "hs_eq64"
        MO_x64_Ne    -> fsLit "hs_ne64"
        MO_I64_Ge    -> fsLit "hs_geInt64"
        MO_I64_Gt    -> fsLit "hs_gtInt64"
        MO_I64_Le    -> fsLit "hs_leInt64"
        MO_I64_Lt    -> fsLit "hs_ltInt64"
        MO_W64_Ge    -> fsLit "hs_geWord64"
        MO_W64_Gt    -> fsLit "hs_gtWord64"
        MO_W64_Le    -> fsLit "hs_leWord64"
        MO_W64_Lt    -> fsLit "hs_ltWord64"

        MO_UF_Conv w -> fsLit $ word2FloatLabel w

        MO_Memcpy _  -> fsLit "memcpy"
        MO_Memset _  -> fsLit "memset"
        MO_Memmove _ -> fsLit "memmove"
        MO_Memcmp _  -> fsLit "memcmp"

        MO_BSwap w   -> fsLit $ bSwapLabel w
        MO_BRev w    -> fsLit $ bRevLabel w
        MO_PopCnt w  -> fsLit $ popCntLabel w
        MO_Pdep w    -> fsLit $ pdepLabel w
        MO_Pext w    -> fsLit $ pextLabel w
        MO_Clz w     -> fsLit $ clzLabel w
        MO_Ctz w     -> fsLit $ ctzLabel w
        MO_AtomicRMW w amop -> fsLit $ atomicRMWLabel w amop
        MO_Cmpxchg w -> fsLit $ cmpxchgLabel w
        MO_Xchg w -> fsLit $ xchgLabel w
        MO_AtomicRead w -> fsLit $ atomicReadLabel w
        MO_AtomicWrite w -> fsLit $ atomicWriteLabel w

        MO_S_Mul2    {}  -> unsupported
        MO_S_QuotRem {}  -> unsupported
        MO_U_QuotRem {}  -> unsupported
        MO_U_QuotRem2 {} -> unsupported
        MO_Add2 {}       -> unsupported
        MO_AddWordC {}   -> unsupported
        MO_SubWordC {}   -> unsupported
        MO_AddIntC {}    -> unsupported
        MO_SubIntC {}    -> unsupported
        MO_U_Mul2 {}     -> unsupported
        MO_ReadBarrier   -> unsupported
        MO_WriteBarrier  -> unsupported
        MO_Touch         -> unsupported
        (MO_Prefetch_Data _) -> unsupported
    where unsupported = panic ("outOfLineCmmOp: " ++ show mop
                            ++ " not supported here")

