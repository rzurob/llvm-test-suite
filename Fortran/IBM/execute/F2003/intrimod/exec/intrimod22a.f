! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f ieee_*.mod xlf_fp_util.mod
! %COMPOPTS: -qhalt=w
! %GROUP: intrimod22a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f xlf_fp_util.mod
! %END
!************************************************************************
!*
!*  Test Case Name   : intrimod22a.f
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module in a module with the same name
!*                     as another NON_INTRINSIC module.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!


!... This module uses the INTRINSIC module 'xlf_fp_util'

       module mod1

         use, intrinsic :: xlf_fp_util
         integer(fpscr_kind), dimension(5) :: flags

         contains
            subroutine sub1(rt_nearest)
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: xlf_fp_util

               real*4 yr
               type(ieee_round_type) :: rtype
               type(ieee_round_type), intent(in) :: rt_nearest
               type(ieee_status_type) :: status_value
               logical :: flag_values(5)

               call ieee_get_flag(ieee_all, flag_values)
               do k = 1, 5
                  if (flag_values(k) .neqv. .false. ) stop 10
               enddo

               call ieee_get_status(status_value)
               call ieee_set_rounding_mode(rt_nearest)
               call ieee_get_rounding_mode(rtype)
               if (rtype /= rt_nearest) stop 15
               yr = ieee_rint(1.1)
               if (yr /= 1.0) stop 16
               call ieee_set_status(status_value)

               call set_fpscr_flags(flags(1))
               call clr_fpscr_flags(flags(5))
               if ( get_fpscr_flags(flags(1)) .eq. 0 ) stop 17
               if ( get_fpscr_flags(flags(5)) .ne. 0 ) stop 18

            end subroutine

       end module



!... This NON_INTRINSIC module is named same as the INTRINSIC module
!... 'xlf_fp_util' which is accessed via use association.

       module xlf_fp_util
          use mod1
       end module


       program intrimod22a

         use, intrinsic :: ieee_arithmetic
         use, non_intrinsic :: xlf_fp_util		! NON_INTRINSIC
         implicit none

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

         call sub1(IEEE_NEAREST)
      end program

