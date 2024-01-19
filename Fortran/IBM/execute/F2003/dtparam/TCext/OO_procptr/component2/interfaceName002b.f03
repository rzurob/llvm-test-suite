! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/interfaceName002b.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is a module
!                              procedure. Unlimited poly, scalar or
!                              array.
!
!                              This test case use explicit interface
!                              implied by use association to call
!                              module procedures. The actual procedure
!                              name has the same name as interface-name.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
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

    contains

    subroutine sub1(b)
        class(*), intent(in) :: b
        select type (b)
            type is (Base(4,*))
                print *, "sub1 Base", b
            type is (Child(4,*))
                print *, "sub1 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(*) :: b
        class(Base(4,:)), allocatable :: func1(:)
        select type (b)
            type is (Base(4,*))
                allocate(func1(5), SOURCE=(/(Base(4,20)(j+b%i),j=1,5)/))
            type is (Child(4,*))
                allocate(func1(8),SOURCE=(/(Child(4,20)(j+b%i,j+b%j),j=1,8)/))
            class default
                error stop 4_4
        end select
    end function
end module

module m2
use m1
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName002b
use m2
    implicit type(Container(4,20)) (c)

    class(*), pointer :: b1

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(4,20)(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4,*))
            print *, "func1 Base", b
        type is (Child(4,*))
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20)(20,22))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4,*))
            print *, "func1 Base", b
        type is (Child(4,*))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
