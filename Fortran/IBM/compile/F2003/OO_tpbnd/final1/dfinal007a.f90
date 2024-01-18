!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: A final
!*                               -subroutine-name is same with
!*                               the binding name of a nopass type-bound
!*                               procedure.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final  :: finalizeBase
    end type

    contains
    subroutine finalizeBase (b1)
       type(base) :: b1
       print *, 'finalizeBase'
    end subroutine

end module

module m1
use m
    type, extends(base) :: child
    contains
       procedure, nopass :: finalizeBase
    end type
end module

end
