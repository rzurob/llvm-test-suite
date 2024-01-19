!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is an internal
!                              procedure. unlimited poly, scalar or
!                              array.
!
!                              This test case is diagnostic.
!                              The actual procedure associated has
!                              different name from interface-name.
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

    interface
        subroutine interfaceSub1(b)
            class(*), intent(in) :: b
        end subroutine

        type(Base) function interfaceFunc1(b)
        import Base
            class(*), pointer, intent(in) :: b
        end function
    end interface

    type, extends(Base) :: Child
        integer j
        procedure(interfaceSub1), pointer, nopass :: pp1
        procedure(interfaceFunc1), pointer, nopass :: pp2
    end type
end module

program interfaceName004b
use m
    type(Child) :: c1

    c1%pp1 => sub1
    c1%pp2 => func1

    contains

    subroutine sub1(b)
        class(*), intent(in) :: b
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
            type is (Child)
                print *, "sub1 Child", b%i, b%j
            class default
                error stop 1_4
        end select
    end subroutine

    type(Base) function func1(b)
        class(*), pointer, intent(in) :: b
        select type (b)
            type is (Base)
                func1 = Base(b%i*2)
            type is (Child)
                func1 = Base(b%i+b%j)
            class default
                error stop 2_4
        end select
    end function
end
