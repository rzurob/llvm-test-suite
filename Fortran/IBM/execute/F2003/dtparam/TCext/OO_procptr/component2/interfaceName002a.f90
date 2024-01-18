! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/component2/interfaceName002a.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is a module
!                              procedure. Poly, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    subroutine sub1(b)
        class(Base(4)), intent(in) :: b
        select type (b)
            type is (Base(4))
                print *, "sub1 Base", b
            type is (Child(4))
                print *, "sub1 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(Base(4)) :: b
        class(Base(4)), pointer :: func1(:)
        select type (b)
            type is (Base(4))
                allocate(func1(5), SOURCE=(/(Base(4)(j+b%i),j=1,5)/))
            type is (Child(4))
                allocate(func1(8),SOURCE=(/(Child(4)(j+b%i,j+b%j),j=1,8)/))
            class default
                error stop 4_4
        end select
    end function
end module

module m2
use m1
    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName002a
use m2
    type(Container(4)) :: c1
    class(Base(4)), pointer :: b1

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(4)(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        type is (Child(4))
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4)(20,22))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        type is (Child(4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
