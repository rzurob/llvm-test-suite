! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_procptr/declaration2/interfaceName004b.f
! opt variations: -qnok -qnol -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either an internal subroutine or
!                              an internal function. Unlimited poly.
!                              Intrinsic or derived type, scalar or
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program interfaceName004b
use m
    interface
        subroutine interfaceSub1(b)
            class(*), intent(in) :: b
        end subroutine

        type(Base(4,20)) function interfaceFunc1(b)
        import Base
            class(*), pointer, intent(in) :: b
        end function
    end interface

    procedure(interfaceSub1), pointer :: pp1
    procedure(interfaceFunc1), pointer :: pp2

    pp1 => sub1
    pp2 => func1

    contains

    subroutine sub1(b)
        class(*), intent(in) :: b
        select type (b)
            type is (Base(4,*))
                print *, "sub1 Base", b
            type is (Child(4,*))
                print *, "sub1 Child", b
            class default
                error stop 1_4
        end select
    end subroutine

    type(Base(4,20)) function func1(b)
        class(*), pointer, intent(in) :: b
        select type (b)
            type is (Base(4,*))
                func1 = Base(4,20)(b%i*2)
            type is (Child(4,*))
                func1 = Base(4,20)(b%i+b%j)
            class default
                error stop 2_4
        end select
    end function
end