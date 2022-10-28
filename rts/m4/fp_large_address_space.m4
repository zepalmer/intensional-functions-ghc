dnl large address space support (see rts/include/rts/storage/MBlock.h)
dnl
dnl Darwin has vm_allocate/vm_protect
dnl Linux has mmap(MAP_NORESERVE)/madv(MADV_DONTNEED)
dnl FreeBSD, Solaris and maybe other have MAP_NORESERVE/MADV_FREE
dnl (They also have MADV_DONTNEED, but it means something else!)
dnl
dnl Windows has VirtualAlloc MEM_RESERVE/MEM_COMMIT, however
dnl it counts page-table space as committed memory, and so quickly
dnl runs out of paging file when we have multiple processes reserving
dnl 1TB of address space, we get the following error:
dnl    VirtualAlloc MEM_RESERVE 1099512676352 bytes failed: The paging file is too small for this operation to complete.
dnl

AC_DEFUN([FP_LARGE_ADDRESS_SPACE],
[
    AC_ARG_ENABLE(large-address-space,
        [AS_HELP_STRING([--enable-large-address-space],
            [Use a single large address space on 64 bit systems (enabled by default on 64 bit platforms)])],
        EnableLargeAddressSpace=$enableval,
        EnableLargeAddressSpace=yes
    )

    use_large_address_space=no
    if test "$ac_cv_sizeof_void_p" -eq 8 ; then
        if test "x$EnableLargeAddressSpace" = "xyes" ; then
            if test "$ghc_host_os" = "darwin" ; then
                use_large_address_space=yes
            elif test "$ghc_host_os" = "openbsd" ; then
                # as of OpenBSD 5.8 (2015), OpenBSD does not support mmap with MAP_NORESERVE.
                # The flag MAP_NORESERVE is supported for source compatibility reasons,
                # but is completely ignored by OS mmap
                      use_large_address_space=no
            elif test "$ghc_host_os" = "mingw32" ; then
                # as of Windows 8.1/Server 2012 windows does no longer allocate the page
                # tabe for reserved memory eagerly. So we are now free to use LAS there too.
                      use_large_address_space=yes
            else
                AC_CHECK_DECLS([MAP_NORESERVE, MADV_FREE, MADV_DONTNEED],[],[],
                    [
                    #include <unistd.h>
                    #include <sys/types.h>
                    #include <sys/mman.h>
                    #include <fcntl.h>
                ])
                if test "$ac_cv_have_decl_MAP_NORESERVE" = "yes" &&
                    test "$ac_cv_have_decl_MADV_FREE" = "yes" ||
                    test "$ac_cv_have_decl_MADV_DONTNEED" = "yes" ; then
                        use_large_address_space=yes
                fi
            fi
        fi
    fi
    if test "$use_large_address_space" = "yes" ; then
       AC_DEFINE([USE_LARGE_ADDRESS_SPACE], [1], [Enable single heap address space support])
    fi
])
