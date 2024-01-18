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
      subroutine check_c_to_f_in(c_arg2) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(IN) :: c_arg2
      end subroutine
   
      subroutine check_c_to_f_out(c_arg2) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(out) :: c_arg2
        print *, c_arg2
        c_arg2 = "MODIFY"
        print *, c_arg2
      end subroutine

      subroutine check_c_to_f_inout(c_arg2) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(inout) :: c_arg2
        print *, c_arg2
        c_arg2 = "MODIFY"
        print *, c_arg2
      end subroutine


