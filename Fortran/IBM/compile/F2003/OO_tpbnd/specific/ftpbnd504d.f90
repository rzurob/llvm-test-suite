!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : specific type bound (binding name reduction is
!                               not allowed: i.e. a public inherited binding can
!                               not be overridden to be private)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

end module

module m1
use m
    type, extends (base) :: child
        character(20) :: name
    end type
end module

module m2
use m1
    type, extends(child) :: gen3
        logical*4 :: flag

        contains
        private
        procedure :: print => printG3  !<-- illegal
    end type

    contains

    subroutine printG3 (b)
        class (gen3), intent(in) :: b
    end subroutine
end module

program ftpbnd504d
end
