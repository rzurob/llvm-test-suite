! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_procptr/declaration2/interfaceName002g.f
! opt variations: -qnok -qnol -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either a module subroutine or
!                              a module function. Unlimited poly.
!                              Intrinsic or derived type, scalar or
!                              array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The dummy arguments are
!                              pointer and are arrays. Abstract type.
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
        class(*), pointer, intent(in) :: b(:,:)
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
        class(*), pointer, intent(in) :: b(:,:)
        class(*), allocatable :: func1
        select type (b)
            type is (Base(4,*,*,4))
                allocate(func1, SOURCE=Base(4,20,20,4)(sum(b%i)))
            type is (Child(4,*,*,4,*,4))
                allocate(func1, SOURCE=Child(4,20,20,4,20,4)(sum(b%i),sum(b%j)))
            class default
                error stop 4_4
        end select
    end function
end module

program interfaceName002g
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    class(*), pointer :: b1(:,:)

    pp1 => sub1
    pp2 => func1

    allocate(b1(4,3), SOURCE=reshape((/(Base(4,20,20,4)(i),i=1,12)/),(/4,3/)))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Base(4,*,*,4))
            print *, "func1 Base", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1(2,5), SOURCE=reshape((/(Child(4,20,20,4,20,4)(-i,i),i=1,10)/),(/2,5/)))
    call pp1(b1)
    select type (b=>pp2(b1))
        type is (Child(4,*,*,4,*,4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
