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
      subroutine check_c_to_f(c_arg1, c_len1, test_no1) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), optional :: c_arg1
        integer(C_INT) c_len1, test_no1
        character(c_len1) c_test
        if(test_no1 .NE. 7) then
        if(c_len1 .NE. LEN(c_arg1)) then

           error STOP 1
        endif
        if(RANK(c_arg1) .NE. 0) then
           error STOP 2
        endif
        if(test_no1 .EQ. 1) then
           c_test = 'C2F__'
        endif
        if(test_no1 .EQ. 2) then
           c_test = 'C2F2F'
        endif
        if(c_arg1 .NE. c_test) then
           print *, c_arg1
           error STOP 3
        endif
        else
           if(PRESENT(c_arg1)) then
               error STOP 7
           endif
        endif
      end subroutine

      subroutine check_c_to_f_to_f(c_arg2, c_len2, test_no2) bind(c)
        use, intrinsic :: iso_c_binding
        interface
         subroutine check_c_to_f(c_arg1, c_len1, test_no1) bind(c)
           use, intrinsic :: iso_c_binding
           character(*), optional :: c_arg1
           integer(C_INT) c_len1, test_no1
         end subroutine
        end interface
        character(*), optional :: c_arg2
        integer(C_INT) c_len2, test_no2
        character(c_len2) c_test
        if(test_no2 .NE. 7) then
         call check_c_to_f(c_arg2, c_len2, test_no2)
        else
         call check_c_to_f(c_len1=c_len2, test_no1=test_no2)
        endif
      end subroutine

      subroutine check_c_to_f_to_c(c_arg3, c_len3, test_no3) bind(c)
        use, intrinsic :: iso_c_binding
        interface
         subroutine check_f_to_c(c_arg1, c_len1, test_no1) bind(c)
           use, intrinsic :: iso_c_binding
           character(*), optional :: c_arg1
           integer(C_INT) c_len1, test_no1
         end subroutine
        end interface
        character(*), optional :: c_arg3
        integer(C_INT) c_len3, test_no3
        character(c_len3) c_test
        if(test_no3 .NE. 7) then
          call check_f_to_c(c_arg3, c_len3, test_no3)
        else
          call check_f_to_c(c_len1=c_len3, test_no1=test_no3)
        endif
      end subroutine

