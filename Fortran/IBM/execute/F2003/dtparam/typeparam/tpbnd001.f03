! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/08/2006
!*
!*  DESCRIPTION                : miscellenous (problem in type bounds)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character(20) :: name = 'default'

        contains

        procedure :: print2 => printBase
        procedure :: print4 => printBase
        procedure :: print8 => printBase
    end type

    type, extends(base) :: child2! (k)
        integer(2) :: id = -1

        contains

        procedure :: print2 => printChild2
    end type

    type, extends(base) :: child4! (k)
        integer(4) :: id = -1

        contains

        procedure :: print4 => printChild4
    end type

    type, extends(base) :: child8! (k)
        integer(8) :: id = -1

        contains

        procedure :: print8 => printChild8
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild2 (b)
        class(child2), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    subroutine printChild4 (b)
        class(child4), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    subroutine printChild8 (b)
        class(child8), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module

program tpbnd001
use m
    class(base), allocatable :: b1
    class(base), pointer :: b2(:), b3

    allocate (child4 :: b1)
    allocate (child8 :: b2(10))
    allocate (child2 :: b3)

    select type (x => b1)
        type is (child4)
            x%name = 'b1 in main'
            x%id = 100

        type is (child8)
            error stop 1_4
        class is (base)
            error stop 2_4
        class default
            error stop 3_4
    end select

    select type (x => b2)
        class is (child4)
            error stop 4_4

        class is (child8)
            x%name = (/('b2:'//char(ichar('0')+i)//' in main', i=0,9)/)
            x%id = 2_8**30 *(/(i*1024, i=1,10)/)

        class is (base)
            error stop 5_4
        class default
            error stop 6_4
    end select

    select type (x => b3)
        class is (child8)
            error stop 7_4
        type is (child4)
            error stop 8_4

        class is (base)
            x%name = 'b3 in main'

        class default
            error stop 9_4
    end select

    !! verify the assignment
    call b1%print4

    do i = 1, 10
        call b2(i)%print8
    end do

    call b3%print2
end
