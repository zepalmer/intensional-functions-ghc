# Configure script template for the Run-time System of GHC
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([GHC run-time system], [1.0.2], [libraries@haskell.org], [rts])

AC_CONFIG_MACRO_DIRS([m4 ../m4])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/rts/Constants.h])

dnl * We require autoconf version 2.69 due to
dnl   https://bugs.ruby-lang.org/issues/8179. Also see #14910.
dnl * We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl * We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
dnl * Using autoconf 2.59 started to give nonsense like this
dnl     #define SIZEOF_CHAR 0
dnl   recently.
AC_PREREQ([2.69])

AC_CONFIG_HEADERS([ghcautoconf.h.autoconf])

# We have to run these unconditionally, but we may discard their
# results in the following code
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

GHC_CONVERT_PLATFORM_PARTS([build], [Build])
GHC_CONVERT_PLATFORM_PARTS([host], [Host])
FPTOOLS_SET_PLATFORM_VARS([build], [Build])
FPTOOLS_SET_PLATFORM_VARS([host], [Host])
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])

AC_CHECK_HEADERS([ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/param.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timerfd.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h utime.h windows.h winsock.h sched.h])

AC_CHECK_FUNCS([getclock getrusage gettimeofday setitimer siginterrupt sysconf times ctime_r sched_setaffinity sched_getaffinity setlocale uselocale])

dnl sys/cpuset.h needs sys/param.h to be included first on FreeBSD 9.1; #7708
AC_CHECK_HEADERS([sys/cpuset.h], [], [],
[[#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
]])

dnl ** check whether a declaration for `environ` is provided by libc.
FP_CHECK_ENVIRON

dnl ** Other RTS features
dnl --------------------------------------------------------------

FP_FIND_LIBDW

FP_FIND_LIBNUMA

FP_LEADING_UNDERSCORE
FP_MUSTTAIL

dnl ** Check for __thread support in the compiler
AC_MSG_CHECKING(for __thread support)
AC_COMPILE_IFELSE(
  [ AC_LANG_SOURCE([[__thread int tester = 0;]]) ],
  [
   AC_MSG_RESULT(yes)
   AC_DEFINE([CC_SUPPORTS_TLS],[1],[Define to 1 if __thread is supported])
  ],
  [
   AC_MSG_RESULT(no)
   AC_DEFINE([CC_SUPPORTS_TLS],[0],[Define to 1 if __thread is supported])
  ])

dnl ** Use MMAP in the runtime linker?
dnl --------------------------------------------------------------

case ${TargetOS} in
    linux|linux-android|freebsd|dragonfly|netbsd|openbsd|kfreebsdgnu|gnu|solaris2)
        RtsLinkerUseMmap=1
        ;;
    darwin|ios|watchos|tvos)
        RtsLinkerUseMmap=1
        ;;
    *)
        # Windows (which doesn't have mmap) and everything else.
        RtsLinkerUseMmap=0
        ;;
    esac

AC_DEFINE_UNQUOTED([RTS_LINKER_USE_MMAP], [$RtsLinkerUseMmap],
                   [Use mmap in the runtime linker])


GHC_ADJUSTORS_METHOD([Target])
AC_SUBST([UseLibffiForAdjustors])
AS_IF(
  [test x"${UseLibffiForAdjustors}" = x"YES"],
  [AC_DEFINE([GHC_LIBFFI_ADJUSTORS],[1],[Should libffi be used for adjustors?])],
  [AC_DEFINE([GHC_LIBFFI_ADJUSTORS],[0],[Should libffi be used for adjustors?])]
)

dnl ** do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** what are the sizes of various types
FP_CHECK_SIZEOF_AND_ALIGNMENT(char)
FP_CHECK_SIZEOF_AND_ALIGNMENT(double)
FP_CHECK_SIZEOF_AND_ALIGNMENT(float)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int)
FP_CHECK_SIZEOF_AND_ALIGNMENT(long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_SIZEOF_AND_ALIGNMENT(long long)
fi
FP_CHECK_SIZEOF_AND_ALIGNMENT(short)
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned char)
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned int)
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned long long)
fi
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned short)
FP_CHECK_SIZEOF_AND_ALIGNMENT(void *)

FP_CHECK_SIZEOF_AND_ALIGNMENT(int8_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint8_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int16_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint16_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int32_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint32_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int64_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint64_t)

dnl ** check for pthreads
FP_CHECK_PTHREADS

dnl ** check for math library
dnl    Keep that check as early as possible.
dnl    as we need to know whether we need libm
dnl    for math functions or not
dnl    (see https://gitlab.haskell.org/ghc/ghc/issues/3730)
AC_CHECK_LIB([m], [atan], [need_libm=1], [need_libm=0])
AC_DEFINE_UNQUOTED([GHC_NEED_LIBM], [$need_libm], [Define to 1 if you need to link with libm])

dnl ** check for librt
AC_CHECK_LIB([rt], [clock_gettime], [need_librt=1], [need_librt=1])
AC_DEFINE_UNQUOTED([GHC_NEED_LIBRT], [$need_librt], [Define to 1 if you need to link with librt])
AC_CHECK_FUNCS(clock_gettime timer_settime)
FP_CHECK_TIMER_CREATE

dnl ** check whether we need -ldl to get dlopen()
AC_CHECK_LIB([dl], [dlopen], [need_libdl=1], [need_libdl=0])
AC_DEFINE_UNQUOTED([GHC_NEED_LIBDL], [$need_libdl], [Define to 1 if you need to link with libdl to get dlopen])
dnl ** check whether we have dlinfo
AC_CHECK_FUNCS([dlinfo])

dnl ** On Linux we should have program_invocation_short_name
AC_CHECK_DECLS([program_invocation_short_name], , ,
[#define _GNU_SOURCE 1
#include <errno.h>])

dnl ** check whether we should use the two-step allocator
FP_LARGE_ADDRESS_SPACE

FP_BFD_SUPPORT

AC_CONFIG_FILES([rts.buildinfo])

AC_OUTPUT

dnl ######################################################################
dnl Generate ghcautoconf.h
dnl ######################################################################

[
mkdir -p include
touch include/ghcautoconf.h
> include/ghcautoconf.h

echo "#if !defined(__GHCAUTOCONF_H__)" >> include/ghcautoconf.h
echo "#define __GHCAUTOCONF_H__" >> include/ghcautoconf.h
# Copy the contents of ghcautoconf.h.autoconf, turning '#define PACKAGE_FOO
# "blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
sed ghcautoconf.h.autoconf \
   -e 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$,\1/* #undef \2 */,' \
   -e '/__GLASGOW_HASKELL/d' \
   -e '/REMOVE ME/d' \
   >> include/ghcautoconf.h
echo "#endif /* __GHCAUTOCONF_H__ */" >> include/ghcautoconf.h
]

dnl ######################################################################
dnl Generate external symbol flags (-Wl,-u...)
dnl ######################################################################

cat $srcdir/external-symbols.list.in \
    | "$CC" -E -P -traditional -Iinclude - -o - \
    | sed '/^$/d' \
    > external-symbols.list \
    || exit 1

mv external-symbols.list external-symbols.tmp
if [[ -n "$LeadingUnderscore" ]]; then
    sed 's/^/  -Wl,-u_/' external-symbols.tmp > external-symbols.list
else
    sed 's/^/  -Wl,-u/' external-symbols.tmp > external-symbols.list
fi
rm -f external-symbols.tmp

dnl ######################################################################
dnl Generate build-info
dnl ######################################################################

cat $srcdir/rts.buildinfo.in | \
    sed -e 's/^  *//' | \
    "$CC" -E -P -traditional - -o - \
    > rts.buildinfo
echo "" >> rts.buildinfo
echo "cc-options: ${CFLAGS}" >> rts.buildinfo
echo "ld-options: ${LDFLAGS}" >> rts.buildinfo
rm -f external-symbols.list
