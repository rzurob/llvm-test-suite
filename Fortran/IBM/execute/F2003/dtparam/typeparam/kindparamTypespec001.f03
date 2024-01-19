! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/06/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in type-spec: allocate
!                               statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends(base) :: child (k)
        integer, kind :: k

        integer(k) :: id

        contains

        procedure, nopass :: print => printChild
    end type

    contains

    subroutine printBase ()
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program kindparamTypespec001
use m
    class(base), allocatable :: b1(:,:)
    class(child(8)), pointer :: b2

    allocate (child(4) :: b1(10, 2))
    allocate (child(8) :: b2)

    b2%id = 2**30 *1024_8

    call b1%print
    call b1(5, 1)%print
    call b2%print

    if (b2%id /= 2_8**40) error stop 1_4
end
