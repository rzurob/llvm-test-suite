! ************************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qhalt=w
! %GROUP: intrimod23.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!************************************************************************
!************************************************************************
!*
!*  FORTRAN TEST CASE            IBM INTERNAL USE ONLY
!*  Test Case Title  : INTRINSIC/NON_INTRINSIC module nature
!*  Test Case Name   : intrimod23.f
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module with the same name a COMMON BLOCK
!*                     In the main program and external subroutine.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!


       program intrimod23

         use, intrinsic :: xlf_fp_util
         use, intrinsic :: ieee_arithmetic
         implicit none 
         type(ieee_round_type) :: rt_nearest
         integer(fpscr_kind), dimension(5) :: flags

!... Common blocks with the same name as INTRINSIC modules

         COMMON /xlf_fp_util/ flags
         COMMON /ieee_arithmetic/ rt_nearest

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

         rt_nearest=IEEE_NEAREST

         call sub1()
      end program



      subroutine sub1()
         use, intrinsic :: ieee_arithmetic
         use, intrinsic :: xlf_fp_util

         COMMON /xlf_fp_util/ flags
         COMMON /ieee_arithmetic/ rt_nearest

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)
         type(ieee_round_type) :: rt_nearest
         integer(fpscr_kind), dimension(5) :: flags

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








