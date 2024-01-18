! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June 25, 2012
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from C, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   NULL Pointer, or corresponding C types
!*
!* Dummy Argument:
!*   optional procedure + some intrinsic types
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mod1
    use, intrinsic :: iso_c_binding
    implicit none

    type, bind(c) ::  derived_1
        integer(c_int_least8_t) i8
        integer(c_int_least16_t) i16
    end type

end module

subroutine sub_testf(func1, arg1, func2, arg2) bind(c)
    use mod1

    type(derived_1), optional :: arg1
    integer(c_int_least32_t) arg2
    optional func2
    interface
	integer(c_size_t) function func1(dt_arg) bind(c)
	    use, intrinsic :: iso_c_binding
	    use mod1
            type (derived_1), optional :: dt_arg
	end function

	logical(c_bool) function func2(a1,a2) bind(c)
	    use, intrinsic :: iso_c_binding
	    integer(c_int_least32_t) a1
	    integer(c_int_least64_t), optional :: a2
	end function
    end interface

    logical(c_bool) res

    if (present(arg1)) then
    	if ( c_sizeof(arg1) /= func1(arg1) ) error stop 10
    else
	if ( func1() /= 0 ) error stop 20
    endif

    if(present(func2)) then
        res = func2(arg2)
	if ( res .eqv. .TRUE. ) error stop 30
    endif

end subroutine

