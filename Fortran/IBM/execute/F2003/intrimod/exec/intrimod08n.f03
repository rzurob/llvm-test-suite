! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use NON_INTRINSIC modules with same name as an
!*                     INTRINSIC module in generic interface within the
!*                     main program.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

       program intrimod08n

         use, non_intrinsic :: ieee_arithmetic
         use, non_intrinsic :: xlf_fp_util

         interface test_ieee
            subroutine sub1(rt_nearest, flags)
               use, non_intrinsic :: ieee_arithmetic
               use, non_intrinsic :: xlf_fp_util
               type(ieee_round_type), intent(in) :: rt_nearest
               integer(fpscr_kind), dimension(5) :: flags
            end subroutine sub1
            subroutine sub2(rt_nearest)
               use, non_intrinsic :: ieee_arithmetic
               type(ieee_round_type), intent(in) :: rt_nearest
            end subroutine sub2
         end interface

         integer(fpscr_kind), dimension(5) :: flags

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)

         call test_ieee(IEEE_NEAREST, flags)
         call test_ieee(IEEE_NEAREST)

      end program


!... Calling some ieee procedures and evaluating the results. The real
!... intrinsic ieee module must have been called.
      subroutine sub1(rt_nearest, flags)
         use, non_intrinsic :: ieee_arithmetic
  	 use, non_intrinsic :: ieee_exceptions
         use, non_intrinsic :: constants_for_ieee
         use, non_intrinsic :: xlf_fp_util

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

         if (ieee_support_datatype(PINF_4) .AND. &
 	     ieee_support_datatype(NINF_4)) then
            if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) error stop 12
         endif

         if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
             if (ieee_is_finite(PHD_4) .neqv. .true.) error stop 13
             if (ieee_is_finite(PTD_4) .neqv. .true.) error stop 14
         endif

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype.rt /= rt_nearest.rt) error stop 15
         yr = ieee_rint(1.1)
         if (yr /= 1.0) error stop 16
         call ieee_set_status(status_value)

!... Testing xlf_fp_util module
         call set_fpscr_flags(flags(1))
         call clr_fpscr_flags(flags(5))
         if ( get_fpscr_flags(flags(1)) .eq. 0 ) error stop 17
         if ( get_fpscr_flags(flags(5)) .eq. 0 ) error stop 18

      end subroutine sub1


      subroutine sub2(rt_nearest)
  	 use, non_intrinsic :: ieee_arithmetic
  	 use, non_intrinsic :: ieee_exceptions
         use, non_intrinsic :: constants_for_ieee

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), intent(in) :: rt_nearest
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)

         call ieee_get_flag(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) error stop 30
         enddo

         if (ieee_support_datatype(PINF_4) .AND. &
 	     ieee_support_datatype(NINF_4)) then
            if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) error stop 32
         endif

         if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
             if (ieee_is_finite(PHD_4) .neqv. .true.) error stop 33
             if (ieee_is_finite(PTD_4) .neqv. .true.) error stop 34
         endif

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         if (rtype.rt /= rt_nearest.rt) error stop 35
         yr = ieee_rint(1.1)
         if (yr /= 1.0) error stop 36
         call ieee_set_status(status_value)

     end subroutine sub2

