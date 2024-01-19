! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type-bound procedure (Test that the
!                               nopass binding is inherited from base type:
!                               object counter)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer, protected :: objectCount = 0

    type base (k)
        integer, kind :: k

        integer(k) :: id

        contains

        procedure, non_overridable, nopass :: objCount => howManyObj
        procedure, nopass :: setUp => addOneObj
        procedure, nopass :: destroy => deleteOneObj
    end type

    type, extends(base) :: child (n)
        integer, len :: n

        character(n) :: name
    end type

    contains

    subroutine addOneObj ()
        objectCount = objectCount + 1
    end subroutine

    subroutine deleteOneObj
        objectCount = objectCount - 1
    end subroutine

    integer function howManyObj()
        howManyObj = objectCount
    end function
end module

program dtpNopass003a
use m
    class(base(4)), pointer :: b1, b2(:)
    class(base(8)), pointer :: b3

    class(child(2,:)), allocatable :: c1(:,:)

    allocate (b1, source=base(4)(100))

    call b1%setUp

    allocate (b2(10), source=[(child(4,10)(i, 'test'), i=10,1,-1)])

    do i = 1, 10
        call b2(i)%setUp
    end do

    allocate(b3, source=child(8,20)(-20, ''))

    call b2%setUp

    allocate (child(2,15) :: c1(2,2))

    do i = 1, 2
        do j = 1, 2
            call c1(i,j)%setUp
        end do
    end do

    if (b1%objCount() /= 16) error stop 1

    call b1%destroy

    deallocate (b1)

    if (b2%objCount() /= 15) error stop 2

    do i = 1, 10
        call b2%destroy
    end do

    deallocate (b2)

    if (c1%objCount() /= 5) error stop 3

    do i = 1, 2
        do j = 1, 2
            call c1%destroy
        end do
    end do

    deallocate (c1)

    if (b3%objCount() /= 1) error stop 4
end
