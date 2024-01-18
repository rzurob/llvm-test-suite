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
          subroutine check_f_to_c_in(c_arg1,n) bind(c)
            use, intrinsic :: iso_c_binding 
            character(*), intent(IN) :: c_arg1(n+1:*)
            integer(C_INT) n 
          end subroutine
          subroutine check_f_to_f_in(c_arg2,n) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(IN) :: c_arg2(n+1:*)
            integer(C_INT) n
          end subroutine
          subroutine check_f_to_c_out(c_arg1,n) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(OUT) :: c_arg1(n+1:*)
            integer(C_INT) n
          end subroutine
          subroutine check_f_to_f_out(c_arg2,n) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(OUT) :: c_arg2(n+1:*)
            integer(C_INT) n
          end subroutine
          subroutine check_f_to_c_inout(c_arg1,n) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(INOUT) :: c_arg1(n+1:*)
            integer(C_INT) n
          end subroutine
          subroutine check_f_to_f_inout(c_arg2,n) bind(c)
            use, intrinsic :: iso_c_binding
            character(*), intent(INOUT) :: c_arg2(n+1:*)
            integer(C_INT) n
          end subroutine

        end interface
      
        character(5) :: a1(5)

        a1 = 'F2C__'         
        call check_f_to_c_in(a1,4)
        call check_f_to_f_in(a1,4)
        
         a1 = 'F2C__'
        call check_f_to_c_out(a1,4)
        call check_f_to_f_out(a1,4)

         a1 = 'F2C__'
        call check_f_to_c_inout(a1,4)
        call check_f_to_f_inout(a1,4)
        
      end program

      subroutine check_f_to_f_in(c_arg2, n) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(IN) :: c_arg2(n+1:*)
        integer(C_INT) n
      end subroutine

      subroutine check_f_to_f_out(c_arg2, n) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(OUT) :: c_arg2(n+1:*)
        integer(C_INT) n
        print *, c_arg2(n)
        c_arg2(n) = "MODIFY"
        print *, c_arg2(n)
      end subroutine

      subroutine check_f_to_f_inout(c_arg2, n) bind(c)
        use, intrinsic :: iso_c_binding
        character(*), intent(INOUT) :: c_arg2(n+1:*)
        integer(C_INT) n
         print *, c_arg2(n)
        c_arg2(n) = "MODIFY"
        print *, c_arg2(n)
      end subroutine


