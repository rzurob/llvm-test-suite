! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f ieee_*.mod xlf_fp_util.mod
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp intrimod21d.f
! %END
!************************************************************************
!*
!*  Test Case Name   : intrimod21d.f
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module with same name as the internal
!*                     subroutine and module procedures is not allowed.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

       module mod1

         use, intrinsic :: ieee_arithmetic
         use, intrinsic :: ieee_exceptions
         use, intrinsic :: xlf_fp_util

         integer(fpscr_kind), dimension(5) :: flags

         contains

!... ERROR: module prodcedure with same name as an INTRINSIC module
	    subroutine xlf_fp_util(ieee_exceptions)
               flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

               call ieee_exceptions(IEEE_NEAREST, flags)
            end subroutine

       end module


       program intrimod21d

         use :: mod1
         use, intrinsic :: ieee_arithmetic
         use, intrinsic :: xlf_fp_util
         implicit none
         external ieee_arithmetic		! External procedure

         call xlf_fp_util(ieee_arithmetic)
         call ieee_arithmetic(IEEE_NEAREST, flags)

         if ( .not. ieee_exceptions(IEEE_NEAREST, flags) ) stop 30
         contains
!... ERROR: An internal function with the same name as ieee intrinsic module
!... ieee_exceptions is use ieee_arithmetic therefore it can not be used
!... as the name of internal procedures.

            logical function ieee_exceptions(rt_nearest, flags)
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: xlf_fp_util
               type(ieee_round_type), intent(in) :: rt_nearest
               integer(fpscr_kind), dimension(5) :: flags

               call ieee_set_rounding_mode(rt_nearest)
               call set_fpscr_flags(flags(1))
               call clr_fpscr_flags(flags(5))

               ieee_exceptions=.true.
           end function

      end program



!... An external subroutine with the same name as ieee intrinsic module

      subroutine ieee_arithmetic(rt_nearest, flags)
         use, intrinsic :: ieee_arithmetic
         use, intrinsic :: ieee_exceptions
         use, intrinsic :: xlf_fp_util

         type(ieee_round_type), intent(in) :: rt_nearest
         integer(fpscr_kind), dimension(5) :: flags

         call ieee_set_rounding_mode(rt_nearest)
         call set_fpscr_flags(flags(1))
         call clr_fpscr_flags(flags(5))

      end subroutine ieee_arithmetic


