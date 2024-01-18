! *********************************************************************
!*  ===================================================================
!*
!*  DATE                : June, 1, 2014
!*  FEATURE             : RTC Master Story:
!*                        C Interop: Assumed-length Character arguments
!*                        (master story) (72333)
!*
!*  FEATURE             : C Interop: Assumed-length Character arguments
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      program assumed_lenght001

        interface
          subroutine check_f_to_c_in(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(IN) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_in(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(IN) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_c_out(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(OUT) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_out(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(OUT) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_c_inout(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(INOUT) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_inout(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(INOUT) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine

        end interface

        character(5) :: a1

        a1 = 'F2C__'
        call check_f_to_c_in(a1, LEN(a1), 1)
        call check_f_to_f_in(a1, LEN(a1), 2)
        call check_f_to_c_out(a1, LEN(a1), 1)
        call check_f_to_f_out(a1, LEN(a1), 2)
        call check_f_to_c_inout(a1, LEN(a1), 1)
        call check_f_to_f_inout(a1, LEN(a1), 2)

      end program

      subroutine check_f_to_f_in(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(IN) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
      end subroutine

      subroutine check_f_to_f_out(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(OUT) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        c_arg2 = "MODIFY"
      end subroutine

      subroutine check_f_to_f_inout(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(INOUT) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        c_arg2 = "MODIFY"
      end subroutine

