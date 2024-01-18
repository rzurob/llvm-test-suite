      !!! Test error-messages for BIND(C) subroutine/function/entry
      !!!   for non-interoperable result and/or arguments.  Specifically:
      !!!
      !!! * result of    BIND(C) sub/fn/entry not interoperable.
      !!! * arguments to BIND(C) sub/fn/entry not interoperable.
      !!! * result and arguments declared explicitly or implicitly.
      !!! * OPTIONAL arguments to BIND(C) sub/fn/entry disallowed.

      ! (by default, test with non-interoperable ARGUMENT type `character*2')
#if ARG == 1
#  define ARG_T character*1
#elif ARG == 2 || !defined(ARG)
#  define ARG_T character*2
#endif

      ! (by default, test with explicit result and argument declarations)
#if IMPL == 1 || !defined(IMPL)  /* explicit type dcl */
#  define ARG_DCL \
        ARG_T j, k, je, ke         ! 4 err: non-interop arg
#elif IMPL == 2                  /* implicit type dcl */
#  define ARG_DCL \
        implicit ARG_T (j, k)      ! 4 err: non-interop arg
#endif

      ! (if undefined, default to -DOPT=1)
#if OPT == 1 || !defined(OPT)  /* dcl type, don't dcl OPTIONAL */
#  define ARG_DCL1 ARG_DCL
#  define ARG_DCL2 \
        ! no optional dcl
#elif OPT == 2                 /* dcl type, then dcl OPTIONAL */
#  define ARG_DCL1 ARG_DCL
#  define ARG_DCL2 \
        optional j, k,  je, ke  ! 4 err: optional arg
#elif OPT == 3                 /* dcl OPTIONAL, then dcl type */
#  define ARG_DCL1 \
        optional j, k,  je, ke  ! 4 err: optional arg
#  define ARG_DCL2 ARG_DCL
#elif OPT == 4                 /* dcl type and OPTIONAL in one dcl */
#  define ARG_DCL1 \
        ARG_T, optional :: j, k, je, ke  ! 8 err: non-interop and optional arg
#  define ARG_DCL2 \
        !
#endif

      !! proc & entry both bind(c), result & args not interop
      !
      !
      subroutine s1 (j, k) bind(c)
        ARG_DCL1
        ARG_DCL2
      entry s1e (je, ke) bind(c)
      end subroutine

      !! proc bind(c) but entry not, result & args not interop
      !
      !
      subroutine s2 (j, k) bind(c)
        ARG_DCL1
        ARG_DCL2
      entry s2e (je, ke)
      end subroutine

      !! entry bind(c) but proc not, result & args not interop
      !
      !
      subroutine s3 (j, k)
        ARG_DCL1
        ARG_DCL2
      entry s3e (je, ke) bind(c)
      end subroutine
