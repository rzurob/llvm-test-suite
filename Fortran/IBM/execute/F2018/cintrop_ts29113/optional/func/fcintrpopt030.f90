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
!*   explicit-shape array of intrinsic type
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

       subroutine sub_arr1(a1, b1) bind(c)
        use iso_c_binding
        implicit none
        integer(c_int_least16_t), optional :: a1(2,1)
        real(c_double), optional :: b1(3,2,1)

        if (present(a1)) then
           print *, a1
        else
           print *, "arg a1 is not present"
        end if

        if (present(b1)) then
           print *, b1
        else
           print *, "arg b1 is not present"
        end if
       end subroutine sub_arr1

       subroutine sub_arr2(a2, b2) bind(c)
        use iso_c_binding
        implicit none
        character(c_char), optional :: a2(3,2,1)
        integer(c_signed_char), optional :: b2(2,1)

        if (present(a2)) then
           print *, a2
        else
           print *, "arg a2 is not present"
        end if

        if (present(b2)) then
           print *, b2
        else
           print *, "arg b2 is not present"
        end if
       end subroutine sub_arr2

       subroutine sub_arr3(a3) bind(c)
        use iso_c_binding
        implicit none
        complex(c_float_complex), optional :: a3(1)

        if (present(a3)) then
           print *, a3
        else
           print *, "arg a3 is not present"
        end if

       end subroutine sub_arr3

