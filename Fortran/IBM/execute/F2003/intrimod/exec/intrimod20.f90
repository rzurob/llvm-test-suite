! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module with same name as the main program
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

!... The main program has the same name as an INTRINSIC module

       program xlf_fp_util

  	 use, intrinsic :: ieee_arithmetic
	 use, intrinsic :: ieee_exceptions
         use, intrinsic :: xlf_fp_util

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)
         integer(fpscr_kind), dimension(5) :: flags

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)


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

       end program
