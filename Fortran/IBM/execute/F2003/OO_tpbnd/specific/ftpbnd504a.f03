! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : specific type bound (inherited binding,
!                               overridden binding and new binding)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id

        contains

        procedure :: print1 => printBase
    end type

    private printBase
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

        contains

        procedure, non_overridable :: print => printChild
    end type

    contains

    subroutine printChild (c)
        class (child), intent(in) :: c

        print *, c%id, c%name
    end subroutine
end module

module m2
use m1
    type, extends(child) :: gen3
        logical*2 :: flag

        contains

        procedure :: print1 => printG3
    end type

    contains

    subroutine printG3 (b)
        class (gen3), intent(in) :: b

        print *, b%id, b%name, b%flag
    end subroutine
end module

use m2
    class (child), allocatable, target :: c1(:)
    class (base), pointer :: b1(:), b2(:)
    class (gen3), allocatable, target :: t1(:)

    allocate (c1(10), t1(10))

    c1%id = (/(i, i=1,10)/)
    c1%name = (/('c1_'//char(ichar('0')+i-1), i=1,10)/)

    !! test 1

    print *, 'test 1'

    b1 => t1(:5)

    select type (b1)
        class is (child)
            b1%id = c1(6:)%id
            b1%name = c1(6:)%name

            select type (b1)
                type is (gen3)
                    b1%flag = (1 < 10)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    do i = 1, 5
        call t1(i)%print
        call t1(i)%print1
    end do

    print *, 'test 2'

    b2 => c1(2:8:2)

    select type (b2)
        type is (child)
            do i = 1, 4
                call b2(i)%print
                call b2(i)%print1
            end do
        class default
            error stop 3_4
    end select
end

