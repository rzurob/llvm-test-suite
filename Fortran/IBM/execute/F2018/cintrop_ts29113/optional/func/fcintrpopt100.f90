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
!* Calling a BIND(C) procedure from Fortran, where the procedure is defined in Fortran
!*
!* Actual Argument:
!*   When have multiple optional dummy arguments in a function or subroutine, supply
!*   with no actual argument, or actual arguments for all the optional ones, or
!*   actual arguments for some of the optional arguments
!*
!* Dummy Argument:
!*   derived type + some intrinsic types
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module bindc_test_mod
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_size_t) :: size

    type, bind(c) :: dt0
         complex(c_float_complex)  :: a(2)
         complex(c_double_complex)  :: b
         complex(c_long_double_complex)  :: c
    end type dt0

    type, bind(c) :: dt1
         type(dt0) :: d(2)
         real(c_long_double) :: e
    end type dt1

end module

program main
    use bindc_test_mod

    interface

        integer(c_size_t) function get_size(position, arg1, arg2, arg3) bind(c)
            use, intrinsic :: iso_c_binding
            use bindc_test_mod , only : dt0, dt1
            integer position
            type(dt0), optional :: arg1
            type(dt1), optional :: arg2
            logical(c_bool), optional :: arg3
        end function get_size

    end interface

    type(dt0) :: arg1_a
    type(dt1) :: arg2_a
    logical(c_bool) :: arg3_a

    size = get_size(1)
    print *, size

    size = get_size(1, arg1_a)
    print *, size

    size = get_size(1, arg2=arg2_a)
    print *, size

    size = get_size(1, arg3=arg3_a)
    print *, size

    size = get_size(1, arg1_a, arg2_a)
    print *, size

    size = get_size(1, arg1_a, arg3=arg3_a)
    print *, size

    size = get_size(1, arg2=arg2_a, arg3=arg3_a)
    print *, size

    size = get_size(1, arg2=arg2_a, arg3=arg3_a, arg1=arg1_a)
    print *, size

    size = get_size(2)
    print *, size

    size = get_size(2, arg1_a)
    print *, size

    size = get_size(2, arg2=arg2_a)
    print *, size

    size = get_size(2, arg3=arg3_a)
    print *, size

    size = get_size(2, arg1_a, arg2_a)
    print *, size

    size = get_size(2, arg1_a, arg3=arg3_a)
    print *, size

    size = get_size(2, arg2=arg2_a, arg3=arg3_a)
    print *, size

    size = get_size(2, arg3=arg3_a, arg2=arg2_a, arg1=arg1_a)
    print *, size

    size = get_size(3)
    print *, size

    size = get_size(3, arg1_a)
    print *, size

    size = get_size(3, arg2=arg2_a)
    print *, size

    size = get_size(3, arg3=arg3_a)
    print *, size

    size = get_size(3, arg1_a, arg2_a)
    print *, size

    size = get_size(3, arg1_a, arg3=arg3_a)
    print *, size

    size = get_size(3, arg2=arg2_a, arg3=arg3_a)
    print *, size

    size = get_size(3, arg2=arg2_a, arg3=arg3_a, arg1=arg1_a)
    print *, size

end


integer(c_size_t) function get_size(position, arg1, arg2, arg3) bind(c)
    use, intrinsic :: iso_c_binding
    use bindc_test_mod , only : dt0, dt1
    integer position
    type(dt0), optional :: arg1
    type(dt1), optional :: arg2
    logical(c_bool), optional :: arg3

    if (present(arg1)) then
       print *, "arg1 present"
    else
       print *, "arg1 not present"
    endif

    if (present(arg2)) then
       print *, "arg2 present"
    else
       print *, "arg2 not present"
    endif

    if (present(arg3)) then
       print *, "arg3 present"
    else
       print *, "arg3 not present"
    endif

    select case (position)
       case (1)
            if (present(arg1)) then
                get_size = c_sizeof(arg1)
            else
                get_size = 0
            endif
       case (2)
            if (present(arg2)) then
                get_size = c_sizeof(arg2)
            else
                get_size = 0
            endif
       case (3)
            if (present(arg3)) then
                get_size = c_sizeof(arg3)
            else
                get_size = 0
       	    endif
    end select

end function get_size

