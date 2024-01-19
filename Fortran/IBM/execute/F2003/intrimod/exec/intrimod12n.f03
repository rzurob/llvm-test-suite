! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Use NON_INTRINSIC modules with rename ad ONLY clause.
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

         use, non_intrinsic :: xlf_fp_util, only : fpk => fpscr_kind

         interface
            subroutine sub1(rt_nearest, flags)
               use, non_intrinsic :: ieee_arithmetic, only: inrt=>ieee_round_type
               use, non_intrinsic :: xlf_fp_util, only : fpk => fpscr_kind
               type(inrt), intent(in) :: rt_nearest
               integer(fpk), dimension(5) :: flags
            end subroutine
         end interface

         integer(fpk), dimension(5) :: flags

       end module

       program intrimod12n

         use mod1
         use, non_intrinsic :: ieee_arithmetic, only: inrt => IEEE_NEAREST
         use, non_intrinsic :: xlf_fp_util, only: fpo => fp_overflow,  &
     &               fpd => fp_div_by_zero, fpv => fp_invalid,     &
     &               fpu => fp_underflow, fpe=> fp_inexact
         implicit none

         flags = (/ fpo, fpd, fpv, fpu, fpe /)
         call sub1(inrt, flags)

      end program


!... Calling some renamed ieee procedures and evaluating the results. The fake
!... non_intrinsic ieee module must have been accessed.
      subroutine sub1(rt_nearest, flags)
         use, non_intrinsic :: ieee_arithmetic, isdt=>ieee_support_datatype,  &
    &         iisf=>ieee_is_finite, isrm=>ieee_set_rounding_mode,         &
    &         irt=>ieee_round_type,         &
    &         iri=>ieee_rint
         use, non_intrinsic :: ieee_exceptions, igf=>ieee_get_flag,       &
     &        igrm=>ieee_get_rounding_mode, ist=>ieee_status_type,        &
     &        iss=>ieee_set_status, igs=>ieee_get_status
         use, non_intrinsic :: constants_for_ieee, P4=>PINF_4, N4=>NINF_4
         use, non_intrinsic :: xlf_fp_util, fpo=>fp_overflow,  &
     &               fpd=>fp_div_by_zero, fpv=>fp_invalid,     &
     &               fpu=>fp_underflow, fpe=>fp_inexact, fpk=>fpscr_kind, &
     &               sff=>set_fpscr_flags, cff=>clr_fpscr_flags,          &
     &               gff=>get_fpscr_flags

         real*4 yr
         type(irt) :: rtype
         type(irt), intent(in) :: rt_nearest
         type(ist) :: status_value
         logical :: flag_values(5)
         integer(fpk), dimension(5) :: flags


         call igf(ieee_all, flag_values)
         do k = 1, 5
            if (flag_values(k) .neqv. .false. ) error stop 10
         enddo

         if (isdt(P4) .AND. isdt(N4)) then
            if (iisf(P4) .OR. iisf(N4)) error stop 12
         endif

         if (isdt(PHD_4) .AND. isdt(PTD_4)) then
             if (iisf(PHD_4) .neqv. .true.) error stop 13
             if (iisf(PTD_4) .neqv. .true.) error stop 14
         endif

         call igs(status_value)
         call isrm(rt_nearest)
         call igrm(rtype)
         yr = iri(1.1)
         if (yr /= 1.0) error stop 16
         call iss(status_value)

         call sff(flags(1))
         call cff(flags(5))
         if ( gff(flags(1)) .eq. 0 ) error stop 17
         if ( gff(flags(5)) .eq. 0 ) error stop 18

      end subroutine sub1

