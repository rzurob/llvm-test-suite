! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2005
!*
!*  DESCRIPTION                : argument association (VALUE attribute on
!                               structure constructor and named constants)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg038a
    type base
        integer id
    end type

    type (base), parameter :: b_const = base(1)

    call printB(base(10))
    call printB(b_const)

    contains

    subroutine printB (b)
        type (base), value :: b

        print *, 'base type: ', b%id
    end subroutine
end

