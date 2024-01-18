! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use INTRINSIC module in a NON_INTRINSIC module with
!*                     the same name.
!*                     Generic interface is used in the NON_INTRINSIC module
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

!.... A NON_INTRINSIC module with the same name as the INTRINSIC module
!.... 'xlf_fp_util'. The INTRINSIC module is refrenced in a generic interface.
       module ieee_arithmetic

         use, intrinsic :: xlf_fp_util
         use, intrinsic :: ieee_arithmetic, only: IEEE_NEAREST

         interface test_ieee
            subroutine sub1(rt_nearest, flags)
               use, intrinsic :: ieee_arithmetic
               use, intrinsic :: xlf_fp_util
               type(ieee_round_type), intent(in) :: rt_nearest
               integer(fpscr_kind), dimension(5) :: flags
            end subroutine sub1
            subroutine sub2(rt_nearest)
               use, intrinsic :: ieee_arithmetic
               type(ieee_round_type), intent(in) :: rt_nearest
            end subroutine sub2
         end interface

         integer(fpscr_kind), dimension(5) :: flags

       end module



       program intrimod34d
         use, non_intrinsic :: ieee_arithmetic      ! NON_INTRINSIC
         implicit none

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

         call test_ieee(IEEE_NEAREST, flags)
         call test_ieee(IEEE_NEAREST)
      end program




      subroutine sub1(rt_nearest, flags)
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

      end subroutine sub1


      subroutine sub2(rt_nearest)
  	 use, intrinsic :: ieee_arithmetic

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), intent(in) :: rt_nearest
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)

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

     end subroutine sub2

