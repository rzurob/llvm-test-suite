! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg -qsmp
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp critsect_nonintrin.f; rm -f *.mod
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : The name of a critical section cannot
!*                               be the same as a nonintrinsic module.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

       Module xlf_fp_util
         integer aa
       end module

       use :: ieee_arithmetic
       use :: xlf_fp_util

       real*4 yr, rr
       call ieee_set_rounding_mode(IEEE_NEAREST)
       yr = 0.

!SMP$  PARALLEL DO private(rr)
          do i = 1, 100
              rr = log(real(i))
!SMP$  CRITICAL (xlf_fp_util)
              yr = yr + ieee_rint(rr)
!SMP$  END CRITICAL (xlf_fp_util)
          end do
!SMP$  END PARALLEL DO

          if (yr /= 360.0) stop 12
       end

