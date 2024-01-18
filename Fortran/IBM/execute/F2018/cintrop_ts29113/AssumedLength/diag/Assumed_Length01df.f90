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
          subroutine check_f_to_f_in(c_arg2) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(IN) :: c_arg2(5)
        end subroutine
        end interface

        character(5) :: a1(5)
        a1 = 'F2C__'
        call check_f_to_f_in(a1)
      end program

      subroutine check_f_to_f_in(c_arg2) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(IN):: c_arg2(5)
        print *, c_arg2
        c_arg2 = "MODIFY"
        print *, c_arg2
      end subroutine
















