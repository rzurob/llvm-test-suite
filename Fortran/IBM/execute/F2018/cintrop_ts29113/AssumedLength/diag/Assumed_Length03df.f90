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
      
      program assumed_lenght001
        interface
          subroutine check_function_allocate(c_arg1) bind(c)
            character(*) :: c_arg1    
          end subroutine
          subroutine check_function_deallocate(c_arg1) bind(c)
            character(*) :: c_arg1    
          end subroutine
        end interface

        call check_function_allocate('Allocation Test')
        call check_function_deallocate('Deallocation Test')

       end program

