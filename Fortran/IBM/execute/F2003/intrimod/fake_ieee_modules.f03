!... These are fake ieee modules

     module ieee_arithmetic

            type ieee_round_type
                sequence
                integer rt
            end type

            type ieee_status_type
                integer :: st = 3
            end type

            type(ieee_round_type), parameter :: IEEE_NEAREST=ieee_round_type(2)

            character*40 :: c1='This is a fake ieee_arithmetic module.'
            contains
                subroutine ieee_set_rounding_mode(rtn)
                   type(ieee_round_type) rtn
                   print *, c1
                end subroutine

                function ieee_rint(r1)
                    real r1, ieee_rint
                    ieee_rint = 1.0
                    print *, "You've called a wrong ieee routine."
                end function ieee_rint

                function ieee_value(r1, tp)
                    real r1, ieee_value
                    integer tp
                    ieee_value = r1 * tp
                    print *, "You've called a wrong ieee routine."
                end function ieee_value

                function ieee_class(r1)
                    real r1, ieee_class
                    ieee_class = r1 / .43342
                    print *, "You've called a wrong ieee routine."
                end function ieee_class

                function ieee_next_after(r1,r2)
                    real r1, r2, ieee_next_after
                    ieee_next_after = r1 * r2
                    print *, "You've called a wrong ieee routine."
                end function ieee_next_after

                function ieee_support_datatype(r4)
                    real*4 r4
                    logical ieee_support_datatype
                    print *, "You've called a wrong ieee routine."
                    ieee_support_datatype=.false.
                end function ieee_support_datatype

                function ieee_is_finite(r4)
                    real*4 r4
                    logical ieee_is_finite
                    print *, "You've called a wrong ieee routine."
                    ieee_is_finite=.false.
                end function ieee_is_finite

        end module ieee_arithmetic

        module ieee_exceptions

            use, non_intrinsic :: ieee_arithmetic
            character*40 :: c2='This is a fake ieee_exceptions module.'
            logical :: ieee_flag=.false.
            integer :: ieee_all=1

            contains
                subroutine ieee_get_rounding_mode(rtn)
                   type(ieee_round_type), intent(out) :: rtn
                   print *, c2
                   rtn=IEEE_NEAREST
                end subroutine

                subroutine ieee_get_status( v1 )
                     type(ieee_status_type) :: v1
                     print *, "You've called a wrong status routine."
                     v1.st = 1313
                end subroutine ieee_get_status

                subroutine ieee_set_status( v1 )
                     type(ieee_status_type) :: v1
                     print *, "You've called a wrong status routine : ", v1
                end subroutine ieee_set_status

                subroutine ieee_set_flag(flg)
                    logical flg

                    print *, "You've called a wrong flag routine."
                    ieee_flag = flg
                end subroutine ieee_set_flag

                subroutine ieee_get_flag(flg, hlt)
                    integer flg
                    logical :: hlt(5)

                    print *, "You've called a wrong flag routine."
                    hlt = .false.
                end subroutine ieee_get_flag

        end module ieee_exceptions

        module xlf_fp_util
            integer, parameter :: FPSCR_KIND=4
            integer, parameter :: fp_overflow=1
            integer, parameter :: fp_div_by_zero=2
            integer, parameter :: fp_invalid=3
            integer, parameter :: fp_underflow=4
            integer, parameter :: fp_inexact=3

            Character*40 :: c2="This is a fake xlf_fp_util module."

            contains

                function get_fpscr_flags(i2)
                    integer i2, get_fpscr_flags
                    print *, c2
                    get_fpscr_flags=1313
                end function get_fpscr_flags

                subroutine set_fpscr_flags(i2)
                    integer i2
                    print *, c2
                end subroutine set_fpscr_flags

                subroutine clr_fpscr_flags(i2)
                    integer i2
                    print *, c2
                end subroutine clr_fpscr_flags

        end module xlf_fp_util

