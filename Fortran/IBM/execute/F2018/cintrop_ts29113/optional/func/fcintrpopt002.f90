! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt002.f
!*
!* PROGRAMMER                   : Ying Zhang
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from C, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*    NULL Pointer, or corresponding C types
!*
!* Dummy Argument:
!*    multi-dimension of some intrinsic types, and multiple optional dummy arguments in one procedure
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      subroutine sub_testf(arg1, arg2, arg3) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int), optional, intent(in) :: arg1(5,5)
        integer(c_short), optional, intent(in) :: arg2(5,5,5)
        integer(c_long), optional, intent(inout) :: arg3(5,5,5,5)
        integer :: i, j, k, l

        if (present(arg1)) then
           print *, arg1
        else
           print *, "arg1 is not present"
        end if

        if (present(arg2)) then
           print *, arg2
        else
           print *, "arg2 is not present"
        end if

        if (present(arg3)) then
	    do i=1, 5          	 
	    do j=1, 5          	 
	    do k=1, 5          	 
	    do l=1, 5          	 
		arg3(l,k,j,i)=0
	    end do
	    end do
	    end do
	    end do
           print *, arg3
        else
           print *, "arg3 is not present"
        end if

      end

      function func_testf(arg1, arg2, arg3) bind(C)
        use iso_c_binding
        implicit none
        integer(c_int) :: func_testf
        real(c_long_double), optional, intent(in) :: arg1(5,5,5)
        real(c_double), optional, intent(out) :: arg2(5,5)
        real(c_float), optional, intent(out)  :: arg3(5)
        integer :: i, j

        func_testf = 0

        if (present(arg1)) then
           print *, arg1
           func_testf = func_testf + 1
        else
           print *, "arg1 is not present"
        end if

        if (present(arg2)) then
            do i=1, 5
                do j=1, 5
                    arg2(j, i)=i*j*1.0
	        end do
            end do
            print *, arg2
            func_testf = func_testf + 1
        else
           print *, "arg2 is not present"
        end if

        if (present(arg3)) then
	    do i=1, 5
		arg3(i)=2.0
	    end do
            print *, arg3
            func_testf = func_testf + 1
        else
           print *, "arg3 is not present"
        end if

      end

