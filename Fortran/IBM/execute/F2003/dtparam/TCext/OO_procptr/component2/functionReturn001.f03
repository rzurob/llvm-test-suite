! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/component2/functionReturn001.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Specify
!                              proc-interface using interface-name.
!                              Poly.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    contains

    subroutine sub1(b)
        class(Base(*,4)), intent(in) :: b
        select type (b)
            type is (Base(*,4))
                print *, "sub1 Base", b
            type is (Child(*,4))
                print *, "sub1 Child", b
            class default
                error stop 1_4
        end select
    end subroutine

    subroutine sub2(b)
        class(Base(*,4)), intent(in) :: b
        select type (b)
            type is (Base(*,4))
                print *, "sub2 Base", b
            type is (Child(*,4))
                print *, "sub2 Child", b
            class default
                error stop 2_4
        end select
    end subroutine

    function func1(b)
        class(Base(*,4)), intent(in) :: b
        procedure(sub1), pointer :: func1

        select type (b)
            type is (Base(*,4))
                func1 => sub1
            type is (Child(*,4))
                func1 => sub2
            class default
                error stop 3_4
        end select
    end function
end module

module m2
use m1
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(sub1), pointer, nopass :: pp1
    end type

    class(Container(4,:)), pointer :: c1
end module

program functionReturn001
use m2
    class(Base(:,4)), pointer :: b1

    allocate(Container(4,20)::c1)
    allocate(b1, SOURCE=Base(20,4)(10))
    c1%pp1 => func1(b1)
    call c1%pp1(b1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4)(20, 30))
    c1%pp1 => func1(b1)
    call c1%pp1(b1)
end
