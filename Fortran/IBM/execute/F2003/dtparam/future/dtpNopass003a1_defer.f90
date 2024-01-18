!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/26/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedure (Test that the
!                               nopass binding is inherited from base type:
!                               object counter.  Override the structure
!                               constructor)
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

    interface base
        procedure genBase4
        procedure genBase8
    end interface

    interface child
        procedure genChild4
        procedure genChild8
    end interface

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

    type(base(4)) function genBase4 (id) result (b4)
        integer(4), intent(in) :: id

        b4%id = id

        call b4%setUp()
    end function

    type(base(8)) function genBase8 (id) result (b8)
        integer(8), intent(in) :: id

        b8%id = id

        call b8%setUp
    end function

    function genChild4 (id, name) result (c4)
        integer(4), intent(in) :: id
        character(*), intent(in) :: name

        type(child(4, name%len)) c4

        c4%id = id
        c4%name = name

        call c4%setUp
    end function

    function genChild8 (id, name) result (c8)
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        type(child(8, name%len)) c8

        c8%id = id
        c8%name = name

        call c8%setUp()
    end function
end module

program dtpNopass003a1
use m
    class (base(4)), pointer :: b1, b2(:)
    class(base(8)), pointer :: b3

    type(child(8,:)), allocatable :: c1(:,:)

    allocate (b1, source=base(100_4))

    allocate (b2(10), source=[(child(i, 'test'), i=1,10)])

    allocate (b3, source=base(-10_8))

    allocate (child(8,10) :: c1(2,2))

    c1(1,1) = child(11_8, 'xlftest101')
    c1(2,1) = child(21_8, 'xlftest101')
    c1(1,2) = child(12_8, 'xlftest101')
    c1(2,2) = child(22_8, 'xlftest101')

    if (c1%objCount() /= 16) error stop 1_4

    !! verify b1, b3, b2 and c1

    if (b1%id /= 100) error stop 2_4

    if (b3%id /= -10) error stop 3_4

    if (any(b2%id /= [(i, i=1,10)])) error stop 4_4

    if (any([c1%id] /= [11,21,12,22])) error stop 5_4

    if (any(c1%name /= 'xlftest101')) error stop 6_4

    select type (x => b2)
        type is (child(4,*))
            if (x%n /= 4) error stop 7_4

            if (any(x%name /= 'test')) error stop 8_4

        class default
            error stop 9_4
    end select

    !! let's get rid of objects

    do i = 1, 10
        call b2(i)%destroy()
    end do

    deallocate (b2)

    if (b1%objCount() /= 6) error stop 10_4

    do i = 1, 2
        do j = 1, 2
            call c1%destroy
        end do
    end do

    deallocate (c1)

    if (b1%objCount() /= 2) error stop 11_4
end
