!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : final subroutines
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : testing final subroutines: final
!*                               subroutines cannot be overidden
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final :: finalizeBase
    end type
    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'finalizeBase'
    end subroutine
end module

module m1
use m
    type, extends(base) :: child
    contains

!* expect error message 1514-596 here
       final :: finalizeBase
    end type
    contains

!* expect error message 1514-245 here
    subroutine finalizeBase (b1)
        class (child), intent(inout) :: b1
        print *, 'finalizeChild'
    end subroutine
end module

end