! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module with same name as the external
!*                     subroutine/function. With interface in a module.
!*                     -qextname is also used.
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

         use, intrinsic :: xlf_fp_util

         interface
            subroutine ieee_arithmetic(rt_nearest, flags)
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: xlf_fp_util
               type(ieee_round_type), intent(in) :: rt_nearest
               integer(fpscr_kind), dimension(5) :: flags
            end subroutine
            logical function xlf_fp_util(rt_nearest,flags)
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: xlf_fp_util
               type(ieee_round_type), intent(in) :: rt_nearest
               integer(fpscr_kind), dimension(5) :: flags
            end function
         end interface

         integer(fpscr_kind), dimension(5) :: flags


       end module


       program intrimod34a

         use :: mod1
         use, intrinsic :: ieee_arithmetic
         use, intrinsic :: xlf_fp_util
         implicit none


         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

         call ieee_arithmetic(IEEE_NEAREST, flags)
         if ( .not. xlf_fp_util(IEEE_NEAREST, flags) ) error stop 20

      end program



!... An external subroutine with the same name as ieee intrinsic module

      subroutine ieee_arithmetic(rt_nearest, flags)
         use, intrinsic :: ieee_arithmetic
         use, intrinsic :: xlf_fp_util

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), intent(in) :: rt_nearest
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)
         integer(fpscr_kind), dimension(5) :: flags


         call ieee_get_flag(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) error stop 10
         enddo

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype /= rt_nearest) error stop 15
         yr = ieee_rint(1.1)
         if (yr /= 1.0) error stop 16
         call ieee_set_status(status_value)

         call set_fpscr_flags(flags(1))
         call clr_fpscr_flags(flags(5))
         if ( get_fpscr_flags(flags(1)) .eq. 0 ) error stop 17
         if ( get_fpscr_flags(flags(5)) .ne. 0 ) error stop 18

      end subroutine ieee_arithmetic



!... An external function with the same name as xlf utility intrinsic module

      logical function xlf_fp_util(rt_nearest, flags)
  	 use, intrinsic :: ieee_arithmetic
         use, intrinsic :: xlf_fp_util

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), intent(in) :: rt_nearest
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)
         integer(fpscr_kind), dimension(5) :: flags

         call ieee_get_flag(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) error stop 30
         enddo

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype /= rt_nearest) error stop 35
         yr = ieee_rint(1.1)
         if (yr /= 1.0) error stop 36
         call ieee_set_status(status_value)

         call set_fpscr_flags(flags(1))
         call clr_fpscr_flags(flags(5))
         if ( get_fpscr_flags(flags(1)) .eq. 0 ) error stop 37
         if ( get_fpscr_flags(flags(5)) .ne. 0 ) error stop 38

         xlf_fp_util=.true.

     end function xlf_fp_util
