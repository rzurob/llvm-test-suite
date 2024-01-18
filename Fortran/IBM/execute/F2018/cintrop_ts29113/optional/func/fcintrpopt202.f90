! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fcintrpopt202.f
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
!* Calling a BIND(C) procedure from Fortran, (where the procedure is defined in C)
!*
!* Actual Argument:
!*   When have multiple optional dummy arguments in a function or subroutine, supply
!*   with no actual argument, or actual arguments for all the optional ones, or
!*   actual arguments for some of the optional arguments
!*
!* Dummy Argument:
!*   procedure + explicit-shape array of structure
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module testmod
  use iso_c_binding
  implicit none

  type, bind(c) :: dt1
      real(c_double) :: a
      character(c_char) :: b
  end type
end module testmod

program testprogram
  use testmod
  implicit none

  interface

    integer(c_int) function realfunc(a, b, c) bind(c)
	use iso_c_binding 
	real(c_float) a 
	character(c_char) b, c
    end function

  end interface
 
  interface
     subroutine c_sub_test1(func, dt_arr) bind(c)
        import dt1

	optional func
        interface
          integer(c_int) function func(arg1, arg2, arg3) bind(c)
	    use iso_c_binding
            real(c_float) arg1
            character(c_char) arg2, arg3
          end function func
        end interface

        type(dt1), optional :: dt_arr(3,3)

    end subroutine
  end interface

  type(dt1) :: dt_test_arr(3,3)
  integer i,j

  do i=1,3
    do j=1,3
	dt_test_arr(i,j)%a = 1.0 * i * j
	dt_test_arr(i,j)%b = 'b'

    enddo
  enddo

  call c_sub_test1()
  call c_sub_test1(realfunc)
  call c_sub_test1(dt_arr=dt_test_arr)
  call c_sub_test1(realfunc, dt_test_arr)

end program 
