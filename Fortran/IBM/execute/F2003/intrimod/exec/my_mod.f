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
              use ieee_arithmetic

              if (ieee_support_datatype(PINF_4) .AND. &
                 ieee_support_datatype(NINF_4)) then
                 if (ieee_is_finite(PINF_4) .OR.      &
                       ieee_is_finite(NINF_4)) error stop 22
              endif

              if (ieee_support_datatype(PHD_4) .AND.  &
                       ieee_support_datatype(PTD_4)) then
                 if (ieee_is_finite(PHD_4) .neqv. .true.) error stop 23
                 if (ieee_is_finite(PTD_4) .neqv. .true.) error stop 24
              endif

           end subroutine
      end module

!... A non_intrinsic module that uses intrinsic/non_intrinsic modules
      module my_mod
         use constants

      contains

         subroutine sub1()
            use ieee_arithmetic
	    use ieee_exceptions
            use xlf_fp_util
            use constants

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
               if (flag_values(k) .neqv. .false. ) error stop 10
            enddo

            call sub2()      ! Call a non_intrinsic module procedure

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

      end module my_mod

