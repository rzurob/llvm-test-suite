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
          subroutine check_f_to_c(c_arg1, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding 
            character(*) :: c_arg1(n/2:*)
            integer(C_INT) c_len, n, extent, test_no 
          end subroutine
          subroutine check_f_to_f(c_arg2, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg2(n/2:*)
            integer(C_INT) c_len, n, extent, test_no
          end subroutine
          subroutine check_f_to_f_to_c(c_arg3, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3(n/2:*)
            integer(C_INT) c_len,n, extent, test_no
          end subroutine
          subroutine check_f_to_f_to_f(c_arg3, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3(n/2:*)
            integer(C_INT) c_len,n, extent, test_no
          end subroutine
          subroutine check_f_to_c_to_f(c_arg3, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg3(n/2:*)
            integer(C_INT) c_len,n, extent, test_no
          end subroutine

        end interface
      
        call test_all()

        contains

        subroutine test_all

        character(5) :: a1(5)

        a1 = 'F2C__'
        call check_f_to_c(a1, LEN(a1), 10, SIZE(a1), 1)

        a1 = 'F2F__'
        call check_f_to_f(a1, LEN(a1), 10, SIZE(a1), 2)

        a1 = 'F2F2C'
        call check_f_to_f_to_c(a1, LEN(a1),10, SIZE(a1), 3)

        a1 = 'F2F2F'
        call check_f_to_f_to_f(a1, LEN(a1),10, SIZE(a1), 4)

        a1 = 'F2C2F'
        call check_f_to_c_to_f(a1, LEN(a1),10, SIZE(a1), 5)
        end subroutine

      end program

      subroutine check_f_to_f(c_arg2, c_len, n,extent, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        character(*) :: c_arg2(n/2:*)
        integer(C_INT) c_len,n, extent, test_no
        character(c_len) c_test
        if(c_len .NE. LEN(c_arg2)) then
           error STOP 1        
        endif
        if(RANK(c_arg2) .NE. 1) then
           error STOP 2
        endif
        if(LBOUND(c_arg2,1) .NE. n/2) then
           print *, LBOUND(c_arg2,1)
           error STOP 3
        endif
        i = n
        if(test_no .EQ. 2) then
           c_test = 'F2F___'
        endif
        if(test_no .EQ. 4) then
          c_test = 'F2F2F'
        endif
        if(test_no .EQ. 5) then
          c_test = 'F2C2F'
        endif

        DO WHILE (i .LE. (extent+n/2-1))          
          if(c_arg2(i) .NE. c_test) then
           error STOP 4
          endif
          i = i+1
        END DO
      end subroutine

      subroutine check_f_to_f_to_c(c_arg3, c_len,n, extent, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_c(c_arg1, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1(n/2:*)
            integer(C_INT) c_len,n, extent, test_no
          end subroutine
        end interface           
        character(*) :: c_arg3(n/2:*)           
        integer(C_INT) c_len,n, extent, test_no 
        call check_f_to_c(c_arg3,LEN(c_arg3),n, extent, test_no)
       end subroutine

      subroutine check_f_to_f_to_f(c_arg3, c_len,n, extent, test_no) bind(c)
        use, intrinsic :: iso_c_binding
        interface
          subroutine check_f_to_f(c_arg1, c_len,n, extent, test_no) bind(c)
            use, intrinsic :: iso_c_binding
            character(*) :: c_arg1(n/2:*)
            integer(C_INT) c_len,n, extent, test_no
          end subroutine
        end interface
        character(*) :: c_arg3(n/2:*)
        integer(C_INT) c_len,n, extent, test_no
        call check_f_to_f(c_arg3,LEN(c_arg3),n, extent, test_no)
       end subroutine








       
