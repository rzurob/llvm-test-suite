!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedure (Test the
!                               dummy-arg's characteristic checking: for
!                               non-passed-object dummy-arg, the type parameters
!                               (kind and assumed) are checked for overriding
!                               binding)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        contains

        procedure, nopass :: p => procBase
    end type


    type, extends(base) :: child1
        contains

        procedure, nopass :: p => procChild1
    end type

    type, extends(base) :: child2
        contains

        procedure, nopass :: p => procChild2
    end type

    contains

    subroutine procBase (b1)
        class(base(4,*)), intent(in) :: b1
    end subroutine

    subroutine procChild1 (b1)
        class(base(8,*)), intent(in) :: b1      !<-- illegal
    end subroutine

    subroutine procChild2 (b1)
        class(base(4,30)), intent(in) :: b1     !<-- illegal
    end subroutine
end module

program dtpOverride002d
end
