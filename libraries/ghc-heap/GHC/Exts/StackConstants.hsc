module GHC.Exts.StackConstants where

import           Prelude

#include "Rts.h"
#undef BLOCK_SIZE
#undef MBLOCK_SIZE
#undef BLOCKS_PER_MBLOCK
#include "DerivedConstants.h"

offsetStgCatchSTMFrameCode :: Int
offsetStgCatchSTMFrameCode = (#const OFFSET_StgCatchSTMFrame_code) + (#size StgHeader)

offsetStgCatchFrameHandler :: Int
offsetStgCatchFrameHandler = (#const OFFSET_StgCatchFrame_handler) + (#size StgHeader)

offsetStgCatchSTMFrameHandler :: Int
offsetStgCatchSTMFrameHandler = (#const OFFSET_StgCatchSTMFrame_handler) + (#size StgHeader)

offsetStgUpdateFrameUpdatee :: Int
offsetStgUpdateFrameUpdatee = (#const OFFSET_StgUpdateFrame_updatee) + (#size StgHeader)

offsetStgAtomicallyFrameCode :: Int
offsetStgAtomicallyFrameCode = (#const OFFSET_StgAtomicallyFrame_code) + (#size StgHeader)

offsetStgAtomicallyFrameResult :: Int
offsetStgAtomicallyFrameResult = (#const OFFSET_StgAtomicallyFrame_result) + (#size StgHeader)

offsetStgCatchRetryFrameRunningAltCode :: Int
offsetStgCatchRetryFrameRunningAltCode = (#const OFFSET_StgCatchRetryFrame_running_alt_code) + (#size StgHeader)

offsetStgCatchRetryFrameRunningFirstCode :: Int
offsetStgCatchRetryFrameRunningFirstCode = (#const OFFSET_StgCatchRetryFrame_first_code) + (#size StgHeader)

offsetStgCatchRetryFrameAltCode :: Int
offsetStgCatchRetryFrameAltCode = (#const OFFSET_StgCatchRetryFrame_alt_code) + (#size StgHeader)
