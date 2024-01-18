! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_procptr/component2/interfaceName002c.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Specify procedure interface using
!                              interface-name, which is a module
!                              procedure. Poly, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The dummy arguments are
!                              pointer.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type

    contains

    subroutine sub1(b)
        class(AbstractParent(4,*)), pointer, intent(in) :: b
        select type (b)
            type is (Base(4,*,*,4))
                print *, "sub1 Base", b
            type is (Child(4,*,*,4,*,4))
                print *, "sub1 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(AbstractParent(4,*)), pointer, intent(in) :: b
        class(AbstractParent(4,:)), pointer :: func1
        select type (b)
            type is (Base(4,*,*,4))
                allocate(func1, SOURCE=Base(4,20,20,4)(b%i*2))
            type is (Child(4,*,*,4,*,4))
                allocate(func1, SOURCE=Child(4,20,20,4,20,4)(b%j,b%i))
            class default
                error stop 4_4
        end select
    end function
end module

program interfaceName002c
use m
    type Container(k4,n4)    ! (4,20)
        integer, kind :: k4
        integer, len  :: n4
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type

    type(Container(4,20)) :: c1
    class(AbstractParent(4,20)), pointer :: b1

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1, SOURCE=Base(4,20,20,4)(10))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4,*,*,4))
            print *, "func1 Base", b
        type is (Child(4,*,*,4,*,4))
            print *, "func1 Child", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(4,20,20,4,20,4)(20,22))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4,*,*,4))
            print *, "func1 Base", b
        type is (Child(4,*,*,4,*,4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
