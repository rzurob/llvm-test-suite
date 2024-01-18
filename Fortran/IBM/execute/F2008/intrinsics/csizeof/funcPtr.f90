! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-01
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - test type(C_FUNPTR) which is interoperable
!*                                 with C function pointer type
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
    interface

        subroutine sub(f,size) bind(c)
            use, intrinsic :: iso_c_binding
            type(C_FUNPTR) :: f
            integer(C_INT) :: size
        end subroutine

        real(C_FLOAT) function extFunc(x) bind(c)
            use, intrinsic :: iso_c_binding
            integer x
        end function extFunc

    end interface

    type(C_FUNPTR) :: fp
    integer(C_INT) :: size

    fp = C_FUNLOC(extFunc)

    call sub(fp,size)

    if ( c_sizeof(fp) /= size ) error stop 10

end

real(C_FLOAT) function extFunc(x) bind(c)
    use, intrinsic :: iso_c_binding
    integer x
    extFunc = x * 8.0
end function extFunc
