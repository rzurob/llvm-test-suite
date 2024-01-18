! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use NON_INTRINSIC ieee modules with ONLY clause.
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

         use, non_intrinsic :: xlf_fp_util, only:fpscr_kind

         interface
            subroutine sub1(rt_nearest, flags)
               use, non_intrinsic :: ieee_arithmetic, only :ieee_round_type
               use, non_intrinsic :: xlf_fp_util, only:fpscr_kind
               type(ieee_round_type), intent(in) :: rt_nearest
               integer(fpscr_kind), dimension(5) :: flags
            end subroutine
         end interface

         integer(fpscr_kind), dimension(5) :: flags

       end module

       program intrimod11n

         use :: mod1
         use, non_intrinsic :: ieee_arithmetic, only : IEEE_NEAREST
         use, non_intrinsic :: xlf_fp_util, only : fp_overflow, fp_div_by_zero, &
     &               fp_invalid, fp_underflow, fp_inexact
         implicit none

         flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &               fp_underflow, fp_inexact /)
         call sub1(IEEE_NEAREST, flags)

      end program


!... Calling some ieee procedures and evaluating the results. The real
!... intrinsic ieee module must have been called.
      subroutine sub1(rt_nearest, flags)
         use, non_intrinsic :: ieee_arithmetic, only: ieee_set_rounding_mode, &
     &        ieee_support_datatype, ieee_is_finite, ieee_round_type, ieee_rint
         use, non_intrinsic :: ieee_exceptions, only: ieee_get_flag, ieee_all,&
     &        ieee_get_rounding_mode, ieee_status_type, ieee_set_status,      &
     &        ieee_get_status
         use, non_intrinsic :: constants_for_ieee, only: PINF_4, NINF_4,      &
     &                                PHD_4, PTD_4
         use, non_intrinsic :: xlf_fp_util, only : fp_overflow, fp_div_by_zero,&
     &               fp_invalid, fp_underflow, fp_inexact, fpscr_kind,         &
     &               set_fpscr_flags, clr_fpscr_flags, get_fpscr_flags

         real*4 yr
         type(ieee_round_type) :: rtype
         type(ieee_round_type), intent(in) :: rt_nearest
         type(ieee_status_type) :: status_value
         logical :: flag_values(5)
         integer(fpscr_kind), dimension(5) :: flags


         call ieee_get_flag(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) stop 10
         enddo

         if (ieee_support_datatype(PINF_4) .AND. &
 	     ieee_support_datatype(NINF_4)) then
            if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) stop 12
         endif

         if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4)) then
             if (ieee_is_finite(PHD_4) .neqv. .true.) stop 13
             if (ieee_is_finite(PTD_4) .neqv. .true.) stop 14
         endif

         call ieee_get_status(status_value)
         call ieee_set_rounding_mode(rt_nearest)
         call ieee_get_rounding_mode(rtype)
         yr = ieee_rint(1.1)
         if (yr /= 1.0) stop 16
         call ieee_set_status(status_value)

!... Testing xlf_fp_util module
         call set_fpscr_flags(flags(1))
         call clr_fpscr_flags(flags(5))
         if ( get_fpscr_flags(flags(1)) .eq. 0 ) stop 17
         if ( get_fpscr_flags(flags(5)) .eq. 0 ) stop 18

      end subroutine sub1

