! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE     : C Interop: Assumed-length Character arguments
!*
!*
!*
!*  PROGRAMMER          : Umme Hunny
!*  DATE                : June, 1, 2014
!*  ORIGIN              : AIX Compiler Development, Toronto Lab
!*  FEATURE             : RTC Master Story:
!*                        C Interop: Assumed-length Character arguments
!*                        (master story) (72333)
!*
!*  FEATURE             : C Interop: Assumed-length Character arguments 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012     
      subroutine check_c_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1        
        endif
        if(RANK(c_arg2) .NE. 0) then
           error STOP 2
        endif
        if(test_no .EQ. 1) then
           c_test = 'C2F__'
        endif
        if(test_no .EQ. 2) then
           c_test = 'C2F2F'
        endif
        if(c_arg2 .NE. c_test) then
           error STOP 3
        endif 
      end subroutine

      subroutine check_c_to_f_to_f(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
         subroutine check_c_to_f(c_arg1, c_len, test_no) bind(c)
           use, intrinsic :: iso_c_binding
           character(*) :: c_arg1
           integer(C_INT) c_len, test_no
         end subroutine
        end interface
        character(*) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        call check_c_to_f(c_arg2, c_len, test_no)
      end subroutine

      subroutine check_c_to_f_to_c(c_arg2, c_len, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
         subroutine check_f_to_c(c_arg1, c_len, test_no) bind(c)
           use, intrinsic :: iso_c_binding
           character(*) :: c_arg1
           integer(C_INT) c_len, test_no
         end subroutine
        end interface
        character(*) :: c_arg2
        integer(C_INT) c_len, test_no
        character(c_len) c_test
        call check_f_to_c(c_arg2, c_len, test_no)
      end subroutine

