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
          function check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1
            integer(C_INT) c_len, test_no, check_f_to_c
          end function
          function check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg2
            integer(C_INT) c_len, test_no, check_f_to_f
          end function
          function check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3
            integer(C_INT) c_len, test_no, check_f_to_f_to_c
          end function
          function check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3
            integer(C_INT) c_len, test_no, check_f_to_f_to_f
          end function
          function check_f_to_c_to_f(c_arg3, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3
            integer(C_INT) c_len, test_no, check_f_to_c_to_f
          end function
        end interface

        character(5) :: a1

        a1 = 'F2C__'
        if(check_f_to_c(a1, LEN(a1), 1) .NE. 1) then
           print*, check_f_to_c(a1, LEN(a1), 1)
           error STOP 1
        endif


        a1 = 'F2F__'
        if( check_f_to_f(a1, LEN(a1), 2) .NE. 2) then
           print *, check_f_to_f(a1, LEN(a1), 2)
           error STOP 2
        endif

        a1 = 'F2F2C'
        if(check_f_to_f_to_c(a1, LEN(a1), 3) .NE. 3) then
           print *, check_f_to_f_to_c(a1, LEN(a1), 3)
           error stop 3
        endif

        a1 = 'F2F2F'
        if(check_f_to_f_to_f(a1, LEN(a1), 4) .NE. 4) then
           print *, check_f_to_f_to_f(a1, LEN(a1), 4)
           error stop 4
        endif

        a1 = 'F2C2F'
        if(check_f_to_c_to_f(a1, LEN(a1), 5) .NE. 5) then
           print *, check_f_to_c_to_f(a1, LEN(a1), 5)
           error stop 5
        endif

      end program

      function check_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg2
        integer(C_INT) c_len, test_no, check_f_to_f
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1
        endif
        if(RANK(c_arg2) .NE. 0) then
           error STOP 2
        endif
        if(test_no .EQ. 2) then
           c_test = 'F2F___'
        endif
        if(test_no .EQ. 4) then
           c_test = 'F2F2F'
        endif
        if(test_no .EQ. 5) then
           c_test = 'F2C2F'
        endif
        if(c_arg2 .NE. c_test) then
           error STOP 3
        endif
        check_f_to_f = test_no
      end function

      function check_f_to_f_to_c(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          function check_f_to_c(c_arg1, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1
            integer(C_INT) c_len, test_no, check_f_to_c
          end function
        end interface
        character(*) :: c_arg3
        integer(C_INT) c_len, test_no, check_f_to_f_to_c
        check_f_to_f_to_c = check_f_to_c(c_arg3,LEN(c_arg3), test_no)
       end function

       function check_f_to_f_to_f(c_arg3, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          function check_f_to_f(c_arg2, c_len, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg2
            integer(C_INT) c_len, test_no, check_f_to_f
          end function
        end interface
        character(*) :: c_arg3
        integer(C_INT) c_len, test_no, check_f_to_f_to_f
        check_f_to_f_to_f = check_f_to_f(c_arg3,LEN(c_arg3), test_no)
       end function










