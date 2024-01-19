! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/31/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (unallocated allocatables used as the
!                               actual arg in intrinsic present())
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character(12), allocatable :: name
    end type

    type, extends(base) :: child
        integer(8), allocatable :: id
    end type
end module


logical function test2 (x)
    character(*), allocatable, intent(in), optional :: x

    test2 = present(x)
end function


program falloc015a2
use m

    interface
        logical function test2 (x)
            character(*), allocatable, intent(in), optional :: x
        end function
    end interface

    class(base), allocatable :: b1_alloc(:,:)

    type (base) b1

    class (*), allocatable :: x1(:)


    if (.not. test2(b1%name)) error stop 1_4
    if (test2 ()) error stop 2_4

    if (test1(x1,b1_alloc) /= 2) error stop 3_4

    if (test1(a = x1) /= 1) error stop 4_4
    if (test1(b = b1_alloc) /= 1) error stop 5_4
    if (test1 () /= 0) error stop 6_4

    contains

    integer function test1 (a, b)
        class(*), optional, allocatable, intent(inout) :: a(:)
        class (base), optional, allocatable,intent(out) :: b(:,:)

        test1 = 0

        if (present(a)) then
            test1 = test1 + 1

            if (allocated(a)) test1 = test1 + 10
        end if

        if (present(b)) then
            test1 = test1 + 1

            if (allocated (b)) test1 = test1 + 10
        end if
    end function
end
