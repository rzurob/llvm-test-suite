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

      module module1
       interface
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1(..)
            integer(C_INT) c_len, test_no
          end subroutine
          module subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg2(..)
            integer(C_INT) c_len, test_no
          end subroutine
          module subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3(..)
            integer(C_INT) c_len, test_no
          end subroutine
          module subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3(..)
            integer(C_INT) c_len, test_no
          end subroutine
           subroutine check_f_to_c_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3(..)
            integer(C_INT) c_len, test_no
          end subroutine

      end interface
      end module

      submodule (module1) submod1
         contains
            module subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg2(..)
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1
        endif
        if(RANK(c_arg2) .NE. 1) then
           error STOP 2
        endif
        if(LBOUND(c_arg2,1) .NE. 1) then
           error STOP 3
        endif
        if(UBOUND(c_arg2,1) .NE. 5) then
           error STOP 4
        endif
        if(SIZE(c_arg2,1) .NE. 5) then
           error STOP 5
        endif
      end subroutine
      module   subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1(..)
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*) :: c_arg3(..)
        integer(C_INT) c_len, test_no
        call check_f_to_c(c_arg3,LEN(c_arg3), test_no)
       end subroutine

      module subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg3(..)
        integer(C_INT) c_len, test_no
        call check_f_to_f(c_arg3,LEN(c_arg3), test_no)
       end subroutine


      end submodule
      program assumed_lenght001
      use module1

      character(1) :: a1(5)

      a1 = 'F2C__'
      call check_f_to_c(a1, LEN(a1), 1)

      a1 = 'F2F__'
      call check_f_to_f(a1, LEN(a1), 2)

      a1 = 'F2F2C'
      call check_f_to_f_to_c(a1, LEN(a1), 3)

      a1 = 'F2F2F'
      call check_f_to_f_to_f(a1, LEN(a1), 4)

      a1 = 'F2C2F'
      call check_f_to_c_to_f(a1, LEN(a1), 5)


      end program

