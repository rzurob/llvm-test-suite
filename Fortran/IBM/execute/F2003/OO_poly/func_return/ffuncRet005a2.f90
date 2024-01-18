!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/14/2005
!*
!*  DESCRIPTION                : poly-function return (abstract type as the
!                               return declared type; generic overrides the
!                               structure constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base
        contains

        procedure(printBase), deferred :: print
    end type

    type, extends(base) :: child
        integer(8) id

        contains

        procedure :: print => printChild
    end type

    interface base
        module procedure makeChildAlloc
    end interface

    interface
        subroutine printBase (b)
        import base
            class(base), intent(in) :: b
        end subroutine
    end interface

    contains

    class(base) function makeChildAlloc (id)
        integer(8), intent(in) :: id
        allocatable makeChildAlloc

        allocate (makeChildAlloc, source=child(id))
    end function

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type, extends(child) :: gen3
        character(20) :: name

        contains

        procedure :: print => printGen3
    end type

    interface base
        module procedure makeGen3Alloc
    end interface

    contains

    subroutine printGen3 (b)
        class (gen3), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    class (base) function makeGen3Alloc (id, name)
        allocatable makeGen3Alloc
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        allocate(makeGen3Alloc, source=gen3(id, name))
    end function
end module

program ffuncRet005a2
use m1
    class (base), allocatable :: b1, b2(:)

    allocate (b1, source=base(100_8))

    allocate (b2(0:2), source=(/base(1_8, 'test 01'), base(2_8, 'test 02'), &
            base(3_8, 'test 03')/))

    call b1%print

    call b2(0)%print
    call b2(1)%print
    call b2(2)%print

    associate (x => base(200_8, 'xlftest'))
        call x%print
    end associate
end
