! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 338884: 2nd case)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        integer id(n)
    end type

    contains

    subroutine test (n)
        type(base(n)) :: temp = base(n)(0)  !<-- illegal
    end subroutine
end module
