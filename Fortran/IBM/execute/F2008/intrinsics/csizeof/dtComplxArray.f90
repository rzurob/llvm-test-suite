! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-18
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - assumed-shape dummy arg array of derived-type
!*                               - components of type complex
!*                               - Fortran main program
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program main
    use, intrinsic :: iso_c_binding
    implicit none
    type, bind(c) :: dTypeB
         complex(C_FLOAT_COMPLEX)  :: a(2)
         complex(C_DOUBLE_COMPLEX)  :: b
         complex(C_LONG_DOUBLE_COMPLEX)  :: c
    end type dTypeB

    type(dTypeB) :: dt1(10,10)

    interface

        integer(C_SIZE_T) function get_sizeB(x) bind(c)
            use, intrinsic :: iso_c_binding
            import dTypeB
            type(dTypeB) x(10,10)
        end function get_sizeB

    end interface

    call sub(dt1)

    contains

        subroutine sub(a)
            type(dTypeB) :: a(10,10)

            print *, get_sizeB(a)
            if ( c_sizeof(a) /= get_sizeB(a) ) error stop 10

        end subroutine

end

