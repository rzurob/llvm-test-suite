! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/02/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 336382)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base! (n)
        real(8) :: data

        contains

        procedure, pass :: p => sub1
    end type

    contains

    subroutine sub1 (b)
        class(base), intent(inout) :: b
    end subroutine
end module

module m1
use m
    type, extends(base) :: child! (m)
        character :: name

        contains

        procedure, nopass :: p => sub2
    end type

    contains

    subroutine sub2 (b)
        class(child), intent(inout) :: b
    end subroutine
end module

end