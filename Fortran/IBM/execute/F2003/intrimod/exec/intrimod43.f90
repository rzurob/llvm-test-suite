! ************************************************************************
!************************************************************************
!*
!*  Created By       : Bahram Chehrazy
!*  DATE             : January, 2004
!*  Description      : Testing backward compatibility with XLF8.1.
!*                     Calling module procedures where the modules are
!*                     compiled with 9.1, but the caller is compiled with
!*                     8.1. -qmodule=mangle81 is used for 9.1 unit.
!*                     No error should be generated.
!*
!*************************************************************************
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/23/04   BC     Initial Version
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!

!.... A non_intrinsic module with a module procedure that uses an intrinsic module.
      module constants

      ! IEEE Single: real(4)
        real(4), parameter :: PINF_4 = z"7f800000"
        real(4), parameter :: NINF_4 = z"ff800000"

      ! IEEE Single: real(4)
        real(4), parameter :: PHD_4 = z"007fffff"
        real(4), parameter :: PTD_4 = z"00000001"

        contains
           subroutine sub2()
              use, intrinsic :: ieee_arithmetic

              if (ieee_support_datatype(PINF_4) .AND. &
                 ieee_support_datatype(NINF_4)) then
                 if (ieee_is_finite(PINF_4) .OR.      &
                       ieee_is_finite(NINF_4)) stop 22
              endif

              if (ieee_support_datatype(PHD_4) .AND.  &
                       ieee_support_datatype(PTD_4)) then
                 if (ieee_is_finite(PHD_4) .neqv. .true.) stop 23
                 if (ieee_is_finite(PTD_4) .neqv. .true.) stop 24
              endif
           end subroutine

      end module


!... A non_intrinsic module that uses intrinsic/non_intrinsic modules
      module my_mod

      contains

         subroutine sub1()
            use, intrinsic :: ieee_arithmetic
            use, intrinsic :: ieee_exceptions
            use, non_intrinsic :: constants
            use, intrinsic :: xlf_fp_util

            real*4 yr
            type(ieee_round_type) :: rtype
            type(ieee_round_type), parameter :: rt_nearest = IEEE_NEAREST
            type(ieee_status_type) :: status_value
            logical :: flag_values(5)
            integer(fpscr_kind), dimension(5) :: flags

            flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
     &            fp_underflow, fp_inexact /)

            call ieee_get_flag(ieee_all, flag_values)
            do k = 1, 5
               if (flag_values(k) .neqv. .false. ) stop 10
            enddo

            if (ieee_support_datatype(PINF_4) .AND. &
                ieee_support_datatype(NINF_4)) then
               if (ieee_is_finite(PINF_4) .OR. ieee_is_finite(NINF_4)) stop 12
            endif
            if (ieee_support_datatype(PHD_4) .AND. ieee_support_datatype(PTD_4))then
                if (ieee_is_finite(PHD_4) .neqv. .true.) stop 13
                if (ieee_is_finite(PTD_4) .neqv. .true.) stop 14
            endif

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

         end subroutine sub1

      end module my_mod

