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
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg1(5)
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg2(5)
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg3(5)
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg3(5)
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_c_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg3(5)
            integer(C_INT) c_len, test_no
          end subroutine
        end interface

        character(5) :: a1(5)

        a1 = 'F2C__'
        call check_f_to_c(c_len=LEN(a1), test_no=1)

        a1 = 'F2F__'
        call check_f_to_f(c_len= LEN(a1),test_no=2)

        a1 = 'F2F2C'
        call check_f_to_f_to_c(c_len= LEN(a1),test_no= 3)

        a1 = 'F2F2F'
        call check_f_to_f_to_f(c_len= LEN(a1),test_no= 4)

        a1 = 'F2C2F'
        call check_f_to_c_to_f(c_len= LEN(a1),test_no= 5)


      end program

      subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), optional :: c_arg2(5)
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        if(PRESENT(c_arg2)) then
           error stop 1
        endif
      end subroutine

      subroutine check_f_to_f_to_c(c_arg3, c_len3, test_no3) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_c(c_arg1, c_len1, test_no1) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg1(5)
            integer(C_INT) c_len1, test_no1
          end subroutine
        end interface
        character(*), optional :: c_arg3(5)
        integer(C_INT) c_len3, test_no3
        call check_f_to_c(c_len1=c_len3, test_no1=test_no3)
       end subroutine

       subroutine check_f_to_f_to_f(c_arg3, c_len3, test_no3) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_f(c_arg1, c_len1, test_no1) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), optional :: c_arg1(5)
            integer(C_INT) c_len1, test_no1
          end subroutine
        end interface
        character(*), optional :: c_arg3(5)
        integer(C_INT) c_len3, test_no3
        call check_f_to_f(c_len1=c_len3, test_no1=test_no3)
       end subroutine












