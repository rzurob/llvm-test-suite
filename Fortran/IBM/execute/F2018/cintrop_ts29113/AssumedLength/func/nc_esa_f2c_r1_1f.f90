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
            character(*), DIMENSION(5) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(5) :: c_arg2
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(5) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(5) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
          subroutine check_f_to_c_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(5) :: c_arg3
            integer(C_INT) c_len, test_no
          end subroutine
        end interface

        character(5) :: a1(5)

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

      subroutine check_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), DIMENSION(5) :: c_arg2
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
        i =1
        if(test_no .EQ. 2) then
           c_test = 'F2F___'
        endif
        if(test_no .EQ. 4) then
           c_test = 'F2F2F'
        endif
        if(test_no .EQ. 5) then
           c_test = 'F2C2F'
        endif
        DO WHILE (i .LE. SIZE(c_arg2,1))
          if(c_arg2(i) .NE. c_test) then
           error STOP 6
          endif
          i = i+1
        END DO
      end subroutine

      subroutine check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(5) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*), DIMENSION(5) :: c_arg3
        integer(C_INT) c_len, test_no
        call check_f_to_c(c_arg3,LEN(c_arg3), test_no)
       end subroutine

       subroutine check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_f(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), DIMENSION(5) :: c_arg1
            integer(C_INT) c_len, test_no
          end subroutine
        end interface
        character(*), DIMENSION(5) :: c_arg3
        integer(C_INT) c_len, test_no
        call check_f_to_f(c_arg3,LEN(c_arg3), test_no)
       end subroutine











