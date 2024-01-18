!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated function is a module function.
!                              Poly and unlimited poly. Intrinsic or
!                              derived type, scalar.
!
!                              Involves abstract type and implicit
!                              typing. Function returns derived type.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function func1(b)
        class(AbstractParent), intent(in) :: b
        type(Base), allocatable :: func1
        select type (b)
            type is (Base)
                allocate(func1, SOURCE=Base(b%i*2))
            type is (Child)
                allocate(func1, SOURCE=Base(b%i+b%j))
            class default
                error stop 1_4
        end select
    end function

    function func2(b)
        class(*), allocatable, intent(in) :: b
        type(Base) :: func2
        select type (b)
            type is (Base)
                func2 = Base(b%i/2)
            type is (Child)
                func2 = Base(b%i-b%j)
            class default
                error stop 2_4
        end select
    end function
end module

program procInterface002b
use m
    implicit type(Base) (b), type(Base) (c)

    procedure(func1), pointer :: bpp1
    procedure(func2), pointer :: cpp2

    type(Base) :: rv1
    class(AbstractParent), pointer :: b1
    class(*), allocatable :: b2

    bpp1 => func1
    cpp2 => func2

    allocate(b1, SOURCE=Child(4,5))
    rv1 = bpp1(b1)
    print *, "Func1", rv1

    allocate(b2, SOURCE=Base(6))
    rv1 = cpp2(b2)
    print *, "Func2", rv1
end
