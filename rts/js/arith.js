//#OPTIONS: CPP
// #define GHCJS_TRACE_ARITH 1

#include <js/rts.h>

#ifdef GHCJS_TRACE_ARITH
function h$logArith() { h$log.apply(h$log,arguments); }
#define TRACE_ARITH(args...) h$logArith(args)
#else
#define TRACE_ARITH(args...)
#endif

#define UN(x) ((x)>>>0)
#define W32(x) (BigInt(x))
#define I32(x) (BigInt(x))
#define W64(h,l) ((BigInt(h) << BigInt(32)) | BigInt(l>>>0))
#define W64h(x) (Number(x >> BigInt(32)) >>> 0)
#define W64l(x) (Number(BigInt.asUintN(32, x)) >>> 0)
#define I64(h,l) ((BigInt(h) << BigInt(32)) | BigInt(l>>>0))
#define I64h(x) (Number(x >> BigInt(32))|0)
#define I64l(x) (Number(BigInt.asUintN(32,x)) >>> 0)
#define RETURN_I64(x) RETURN_UBX_TUP2(I64h(x), I64l(x))
#define RETURN_W64(x) RETURN_UBX_TUP2(W64h(x), W64l(x))
#define RETURN_W32(x) return Number(x)

function h$hs_quotWord64(h1,l1,h2,l2) {
  var a = W64(h1,l1);
  var b = W64(h2,l2);
  var r = BigInt.asUintN(64, a / b);
  TRACE_ARITH("Word64: " + a + " / " + b + " ==> " + r);
  RETURN_W64(r);
}

function h$hs_remWord64(h1,l1,h2,l2) {
  var a = W64(h1,l1);
  var b = W64(h2,l2);
  var r = BigInt.asUintN(64, a % b);
  TRACE_ARITH("Word64: " + a + " % " + b + " ==> " + r);
  RETURN_W64(r);
}

function h$hs_timesWord64(h1,l1,h2,l2) {
  var a = W64(h1,l1);
  var b = W64(h2,l2);
  var r = BigInt.asUintN(64, a * b);
  TRACE_ARITH("Word64: " + a + " * " + b + " ==> " + r);
  RETURN_W64(r);
}

function h$hs_minusWord64(h1,l1,h2,l2) {
  var a = (BigInt(h1) << BigInt(32)) | BigInt(l1>>>0);
  var b = (BigInt(h2) << BigInt(32)) | BigInt(l2>>>0);
  var r = BigInt.asUintN(64, a - b);
  TRACE_ARITH("Word64: " + a + " - " + b + " ==> " + r);
  RETURN_W64(r);
}

function h$hs_plusWord64(h1,l1,h2,l2) {
  var a = W64(h1,l1);
  var b = W64(h2,l2);
  var r = BigInt.asUintN(64, a + b);
  TRACE_ARITH("Word64: " + a + " + " + b + " ==> " + r);
  RETURN_W64(r);
}

function h$hs_timesInt64(h1,l1,h2,l2) {
  var a = I64(h1,l1);
  var b = I64(h2,l2);
  var r = BigInt.asIntN(64, a * b);
  TRACE_ARITH("Int64: " + a + " * " + b + " ==> " + r);
  RETURN_I64(r);
}

function h$hs_quotInt64(h1,l1,h2,l2) {
  var a = I64(h1,l1);
  var b = I64(h2,l2);
  var r = BigInt.asIntN(64, a / b);
  TRACE_ARITH("Int64: " + a + " / " + b + " ==> " + r);
  RETURN_I64(r);
}

function h$hs_remInt64(h1,l1,h2,l2) {
  var a = I64(h1,l1);
  var b = I64(h2,l2);
  var r = BigInt.asIntN(64, a % b);
  TRACE_ARITH("Int64: " + a + " % " + b + " ==> " + r);
  RETURN_I64(r);
}

function h$hs_plusInt64(h1,l1,h2,l2) {
  var a = I64(h1,l1);
  var b = I64(h2,l2);
  var r = BigInt.asIntN(64, a + b);
  TRACE_ARITH("Int64: " + a + " + " + b + " ==> " + r);
  RETURN_I64(r);
}

function h$hs_minusInt64(h1,l1,h2,l2) {
  var a = I64(h1,l1);
  var b = I64(h2,l2);
  var r = BigInt.asIntN(64, a - b);
  TRACE_ARITH("Int64: " + a + " - " + b + " ==> " + r);
  RETURN_I64(r);
}

function h$hs_uncheckedShiftLWord64(h,l,n) {
  var rh, rl;

  n &= 63;
  if (n == 0) {
    rh = h;
    rl = l;
  } else if (n === 32) {
    rh = l;
    rl = 0;
  } else if (n < 32) {
    rh = UN((h << n) | (l >>> (32 - n)));
    rl = UN(l << n);
  } else {
    rh = UN(l << (n - 32));
    rl = 0;
  }

  TRACE_ARITH("Word64: " + W64(h,l) + " << " + n + " ==> " + W64(rh,rl));
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_uncheckedShiftRWord64(h,l,n) {
  var rh, rl;

  n &= 63;
  if(n == 0) {
    rh = h;
    rl = l;
  } else if(n === 32) {
    rh = 0;
    rl = h;
  } else if(n < 32) {
    rh = h >>> n;
    rl = UN((l >>> n ) | (h << (32-n)));
  } else {
    rh = 0;
    rl = h >>> (n-32);
  }
  TRACE_ARITH("Word64: " + W64(h,l) + " >>> " + n + " ==> " + W64(rh,rl));
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_uncheckedShiftLLInt64(h,l,n) {
  var rh,rl;

  n &= 63;
  if (n == 0) {
    rh = h;
    rl = l;
  } else if (n === 32) {
    rh = l|0;
    rl = 0;
  } else if (n < 32) {
    rh = (h << n) | (l >>> (32 - n));
    rl = UN(l << n);
  } else {
    rh = l << (n - 32);
    rl = 0;
  }

  TRACE_ARITH("Int64: " + W64(h,l) + " << " + n + " ==> " + W64(rh,rl));
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_uncheckedShiftRAInt64(h,l,n) {
  var rh,rl;

  n &= 63;
  if (n == 0) {
    rh = h;
    rl = l;
  } else if (n < 32) {
    rh = h >> n;
    rl = UN((l >>> n) | UN(h << (32 - n)));
  } else {
    rh = h >= 0 ? 0 : -1;
    rl = UN(h >> (n - 32));
  }

  TRACE_ARITH("Int64: " + W64(h,l) + " >> " + n + " ==> " + W64(rh,rl));
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_uncheckedShiftRLInt64(h,l,n) {
  var rh,rl;

  n &= 63;
  if(n == 0) {
    rh = h;
    rl = l;
  } else if(n == 32) {
    rh = 0;
    rl = UN(h);
  } else if(n < 32) {
    rh = h >>> n;
    rl = UN((l >>> n) | (h << (32-n)));
  } else {
    rh = 0;
    rl = h >>> (n-32);
  }

  TRACE_ARITH("Int64: " + W64(h,l) + " >>> " + n + " ==> " + W64(rh,rl));
  RETURN_UBX_TUP2(rh,rl);
}

var h$mulInt32 = Math.imul;

// Compute product of two Ints. Returns (nh,ch,cl)
// where (ch,cl) are the two parts of the 64-bit result
// and nh is 0 if ch can be safely dropped (i.e. it's a sign-extension of cl).
function h$hs_timesInt2(l1,l2) {
  var a = I32(l1);
  var b = I32(l2);
  var r = BigInt.asIntN(64, a * b);
  TRACE_ARITH("Int32: " + a + " * " + b + " ==> " + r + " (Int64)");

  var rh = I64h(r);
  var rl = I64l(r)|0;
  var nh = ((rh === 0 && rl >= 0) || (rh === -1 && rl < 0)) ? 0 : 1;
  RETURN_UBX_TUP3(nh, rh, rl);
}


function h$mulWord32(l1,l2) {
  var a = W32(l1);
  var b = W32(l2);
  var r = BigInt.asUintN(32, a * b);
  TRACE_ARITH("Word32: " + a + " * " + b + " ==> " + r);
  RETURN_W32(r);
}

function h$mul2Word32(l1,l2) {
  var a = W32(l1);
  var b = W32(l2);
  var r = BigInt.asUintN(64, a * b);
  TRACE_ARITH("Word32: " + a + " * " + b + " ==> " + r + " (Word64)");
  RETURN_W64(r);
}

function h$quotWord32(n,d) {
  var a = W32(n);
  var b = W32(d);
  var r = BigInt.asUintN(32, a / b);
  TRACE_ARITH("Word32: " + a + " / " + b + " ==> " + r);
  RETURN_W32(r);
}

function h$remWord32(n,d) {
  var a = W32(n);
  var b = W32(d);
  var r = BigInt.asUintN(32, a % b);
  TRACE_ARITH("Word32: " + a + " % " + b + " ==> " + r);
  RETURN_W32(r);
}

function h$quotRemWord32(n,d) {
  var a = W32(n);
  var b = W32(d);
  var q = BigInt.asUintN(32, a / b);
  var r = BigInt.asUintN(32, a % b);
  TRACE_ARITH("Word32: " + a + " `quotRem` " + b + " ==> (" + q + ", " + r + ")");
  RETURN_UBX_TUP2(Number(q),Number(r));
}

function h$quotRem2Word32(nh,nl,d) {
  var a = W64(nh,nl);
  var b = W32(d);
  var q = BigInt.asUintN(32, a / b);
  var r = BigInt.asUintN(32, a % b);
  TRACE_ARITH("Word32: " + a + " `quotRem2` " + b + " ==> (" + q + ", " + r + ")");
  RETURN_UBX_TUP2(Number(q),Number(r));
}

function h$wordAdd2(l1,l2) {
  var a = W32(l1);
  var b = W32(l2);
  var r = BigInt.asUintN(64, a + b);
  TRACE_ARITH("Word32: " + a + " + " + b + " ==> " + r + " (Word64)");
  RETURN_W64(r);
}

function h$isDoubleNegativeZero(d) {
  TRACE_ARITH("isDoubleNegativeZero: " + d);
  return (d===0 && (1/d) === -Infinity) ? 1 : 0;
}

function h$isFloatNegativeZero(d) {
  TRACE_ARITH("isFloatNegativeZero: " + d);
  return (d===0 && (1/d) === -Infinity) ? 1 : 0;
}

function h$isDoubleInfinite(d) {
  return (d === Number.NEGATIVE_INFINITY || d === Number.POSITIVE_INFINITY) ? 1 : 0;
}

function h$isFloatInfinite(d) {
  return (d === Number.NEGATIVE_INFINITY || d === Number.POSITIVE_INFINITY) ? 1 : 0;
}

function h$isFloatFinite(d) {
  return (d !== Number.NEGATIVE_INFINITY && d !== Number.POSITIVE_INFINITY && !isNaN(d)) ? 1 : 0;
}

function h$isDoubleFinite(d) {
  return (d !== Number.NEGATIVE_INFINITY && d !== Number.POSITIVE_INFINITY && !isNaN(d)) ? 1 : 0;
}

function h$isDoubleNaN(d) {
  return (isNaN(d)) ? 1 : 0;
}

function h$isFloatNaN(d) {
  return (isNaN(d)) ? 1 : 0;
}

function h$isDoubleDenormalized(d) {
  return (d !== 0 && Math.abs(d) < 2.2250738585072014e-308) ? 1 : 0;
}

function h$isFloatDenormalized(d) {
  return (d !== 0 && Math.abs(d) < 2.2250738585072014e-308) ? 1 : 0;
}

var h$convertBuffer = new ArrayBuffer(8);
var h$convertDouble = new Float64Array(h$convertBuffer);
var h$convertFloat  = new Float32Array(h$convertBuffer);
var h$convertInt    = new Int32Array(h$convertBuffer);

// use direct inspection through typed array for decoding floating point numbers if this test gives
// the expected answer. fixme: does this test catch all non-ieee or weird endianness situations?
h$convertFloat[0] = 0.75;
// h$convertFloat[0] = 1/0; // to force using fallbacks
var h$decodeFloatInt   = h$convertInt[0] === 1061158912 ? h$decodeFloatIntArray   : h$decodeFloatIntFallback;
var h$decodeDouble2Int = h$convertInt[0] === 1061158912 ? h$decodeDouble2IntArray : h$decodeDouble2IntFallback;

function h$decodeFloatIntArray(d) {
    TRACE_ARITH("decodeFloatIntArray: " + d);
    if(isNaN(d)) {
        RETURN_UBX_TUP2(-12582912, 105);
    }
    h$convertFloat[0] = d;
    var i = h$convertInt[0];
    var exp = (i >> 23) & 0xff;
    var sgn = 2 * (i >> 31) + 1;
    var s   = i&8388607;
    if(exp === 0) { // zero or denormal
        if(s === 0) {
            TRACE_ARITH("decodeFloatIntArray s: 0 e: 0");
            RETURN_UBX_TUP2(0, 0);
        } else {
            h$convertFloat[0] = d*8388608;
            i = h$convertInt[0];
            TRACE_ARITH("decodeFloatIntArray s: " + (sgn * (i&8388607)) +  " e: " + ((i&2139095040) >> 23) - 173);
            RETURN_UBX_TUP2(sgn*(i&8388607), ((i&2139095040) >> 23) - 173)
        }
    } else {
      TRACE_ARITH("decodeFloatIntArray s: " + (sgn * (s|8388608)) +  " e: " + (exp-150));
      RETURN_UBX_TUP2(sgn * (s|8388608), exp - 150);
    }
}

function h$decodeFloatIntFallback(d) {
    TRACE_ARITH("decodeFloatIntFallback: " + d);
    if(isNaN(d)) {
      RETURN_UBX_TUP2(-12582912, 105);
    }
    var ret0, ret1;
    CALL_UBX_TUP2(ret0, ret1, h$integer_cmm_decodeDoublezhFallback(d));
    var exponent = ret0 + 29;
    var significand = ret1.shiftRight(28).add(h$bigOne).shiftRight(1).intValue();
    if(exponent > 105) {
      exponent = 105;
      significand = d > 0 ? 8388608 : -8388608;
    } else if(exponent < -151 || significand === 0) {
      significand = 0;
      exponent = 0;
    }
    TRACE_ARITH("decodeFloatIntFallback s: " + significand + " e: " + exponent);
    RETURN_UBX_TUP2(significand, exponent);
}

function h$decodeDouble2IntArray(d) {
    TRACE_ARITH("decodeDouble2IntArray: " + d);
    if(isNaN(d)) {
	RETURN_UBX_TUP4(1, -1572864, 0, 972);
    }
    h$convertDouble[0] = d;
  TRACE_ARITH("decodeDouble2IntArray binary: " + h$convertInt[0].toString(2) + " " + h$convertInt[1].toString(2));
    var i1 = h$convertInt[1];
    var ret1, ret2 = h$convertInt[0], ret3;
    var exp = (i1&2146435072)>>>20;
  if(exp === 0) { // denormal or zero
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
    TRACE_ARITH("decodeDouble2IntArray: exp: " + ret3 + " significand: " + ret1 + " " + ret2);
    RETURN_UBX_TUP4(i1<0?-1:1,ret1,ret2,ret3);
}

function h$decodeDouble2IntFallback(d) {
    TRACE_ARITH("decodeDouble2IntFallback: " + d);
    if(isNaN(d)) {
	RETURN_UBX_TUP4(1,-1572864,0,972);
    }
    var exponent, significand;
    CALL_UBX_TUP2(exponent, significand, h$integer_cmm_decodeDoublezhFallback(d));
    var sign = d<0?-1:1;
    var s = significand.abs();
    var ret1 = s.shiftRight(32).intValue();
    var ret2 = s.intValue();
    var ret3 = exponent;
    TRACE_ARITH("decodeDouble2IntFallback: exp: " + ret3 + " significand: " + ret1 + " " + ret2);
    RETURN_UBX_TUP4(sign, ret1, ret2, ret3);
}

// round .5 to nearest even number
function h$rintDouble(a) {
  var rounda = Math.round(a);
  if(a >= 0) {
    if(a%1===0.5 && rounda%2===1) { // tie
      return rounda-1;
    } else {
      return rounda;
    }
  } else {
    if(a%1===-0.5 && rounda%2===-1) { // tie
      return rounda-1;
    } else {
      return rounda;
    }
  }
}
var h$rintFloat = h$rintDouble;

function h$acos(d) { return Math.acos(d); }
function h$acosf(f) { return Math.acos(f); }

function h$asin(d) { return Math.asin(d); }
function h$asinf(f) { return Math.asin(f); }

function h$atan(d) { return Math.atan(d); }
function h$atanf(f) { return Math.atan(f); }

function h$atan2(x,y) { return Math.atan2(x,y); }
function h$atan2f(x,y) { return Math.atan2(x,y); }

function h$cos(d) { return Math.cos(d); }
function h$cosf(f) { return Math.cos(f); }

function h$sin(d) { return Math.sin(d); }
function h$sinf(f) { return Math.sin(f); }

function h$tan(d) { return Math.tan(d); }
function h$tanf(f) { return Math.tan(f); }

function h$cosh(d) { return (Math.exp(d)+Math.exp(-d))/2; }
function h$coshf(f) { return h$cosh(f); }

function h$sinh(d) { return (Math.exp(d)-Math.exp(-d))/2; }
function h$sinhf(f) { return h$sinh(f); }

function h$tanh(d) { return (Math.exp(2*d)-1)/(Math.exp(2*d)+1); }
function h$tanhf(f) { return h$tanh(f); }

var h$popCntTab =
   [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8];

function h$popCnt32(x) {
   return h$popCntTab[x&0xFF] +
          h$popCntTab[(x>>>8)&0xFF] +
          h$popCntTab[(x>>>16)&0xFF] +
          h$popCntTab[(x>>>24)&0xFF];
}

function h$popCnt64(x1,x2) {
   return h$popCntTab[x1&0xFF] +
          h$popCntTab[(x1>>>8)&0xFF] +
          h$popCntTab[(x1>>>16)&0xFF] +
          h$popCntTab[(x1>>>24)&0xFF] +
          h$popCntTab[x2&0xFF] +
          h$popCntTab[(x2>>>8)&0xFF] +
          h$popCntTab[(x2>>>16)&0xFF] +
          h$popCntTab[(x2>>>24)&0xFF];
}

function h$reverseWord(w) {
  /* Reverse the bits in a 32-bit word this trick comes from
   * https://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel This
   * method should use a bit more memory than other methods, but we choose it
   * because it does not rely on any 64bit multiplication or look up tables.
   * Note that this could be expressed in the Haskell EDSL, but we choose to not
   * do that for improved sharing in the JIT. Should be O(lg n)
   */
  var r = w;
  r = ((r >>> 1) & 0x55555555)   | ((r & 0x55555555) << 1);  // swap odd and even bits
  r = ((r >>> 2) & 0x33333333)   | ((r & 0x33333333) << 2);  // swap consecutive pairs
  r = ((r >>> 4) & 0x0F0F0F0F)   | ((r & 0x0F0F0F0F) << 4);  // swap nibbles
  r = ((r >>> 8) & 0x00FF00FF)   | ((r & 0x00FF00FF) << 8);  // swap bytes
  r = ( r >>> 16             )   | ( r               << 16); // swap 2-byte long pairs
  r = r >>> 0;                                              // ensure w is unsigned
  return r;
}

function h$bswap64(x1,x2) {
  RETURN_UBX_TUP2(UN((x2 >>> 24) | (x2 << 24) | ((x2 & 0xFF00) << 8) | ((x2 & 0xFF0000) >> 8))
                 ,UN((x1 >>> 24) | (x1 << 24) | ((x1 & 0xFF00) << 8) | ((x1 & 0xFF0000) >> 8)));
}

var h$clz32 = Math.clz32 || function(x) {
    if (x < 0)   return 0;
    if (x === 0) return 32;
    return 31 - ((Math.log(x) / Math.LN2) | 0);
}
function h$clz8(x) {
    return h$clz32(x&255)-24;
}
function h$clz16(x) {
    return h$clz32(x&65535)-16;
}

function h$clz64(x1,x2) {
    return (x1 === 0) ? 32 + h$clz32(x2) : h$clz32(x1);
}

var h$ctz32tbl = [32,0,1,26,2,23,27,0,3,16,24,30,28,11,0,13,4,7,17,0,25,22,31,15,29,10,12,6,0,21,14,9,5,20,8,19,18,0,0,0,0,0,31];
function h$ctz32(x) {
    return h$ctz32tbl[((x&-x)%37)&63];
}
function h$ctz16(x) {
    return h$ctz32(x|65536);
}
function h$ctz8(x) {
    return h$ctz32(x|256);
}
function h$ctz64(x1,x2) {
    return (x2 === 0) ? 32 + h$ctz32(x1) : h$ctz32(x2);
}

var h$fround            = null;
var h$truncateFloat_buf = null;
if(typeof Math.fround === 'function') {
  h$fround = function(f) {
    TRACE_ARITH("fround (native): " + f);
    return Math.fround(f);
  }
} else {
  h$fround = function(f) {
    TRACE_ARITH("fround (buffer): " + f);
    if(!h$truncateFloat_buf) h$truncateFloat_buf = new Float32Array(1);
    h$truncateFloat_buf[0] = f;
    return h$truncateFloat_buf[0];
  }
}

function h$decodeDoubleInt64(d) {
  TRACE_ARITH("decodeDoubleInt64: " + d);
  if(isNaN(d)) {
    RETURN_UBX_TUP3(972, -1572864, 0);
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) { // denormal or zero
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  // negate mantissa for negative input
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
      // ret2 = 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  // prim ubx tup returns don't return the first value!
  RETURN_UBX_TUP3(ret3,ret1,ret2);
}

function h$__int_encodeDouble(j,e) {
  if (!j) return 0;
  return (j|0) ** (e|0);
}

function h$__word_encodeDouble(j,e) {
  if (!j) return 0;
  return (j>>>0) ** (e|0);
}

function h$__int_encodeFloat(j,e) {
  if (!j) return 0;
  return h$fround((j|0) ** (e|0));
}

function h$__word_encodeFloat(j,e) {
  if (!j) return 0;
  return h$fround((j>>>0) ** (e|0));
}
