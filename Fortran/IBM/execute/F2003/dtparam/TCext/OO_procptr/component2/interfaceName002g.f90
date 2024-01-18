! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/component2/interfaceName002g.f
! opt variations: -qnok -ql -qreuse=none

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
!                              procedure. Unlimited poly, scalar or
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    subroutine sub1(b)
        class(*), pointer, intent(in) :: b(:,:)
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
        class(*), pointer, intent(in) :: b(:,:)
        class(*), allocatable :: func1
        select type (b)
            type is (Base(4))
                allocate(func1, SOURCE=Base(4)(sum(b%i)))
            type is (Child(4))
                allocate(func1, SOURCE=Child(4)(sum(b%i),sum(b%j)))
            class default
                error stop 4_4
        end select
    end function
end module

program interfaceName002g
use m
    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type

    class(Container(4)), pointer :: c1
    class(*), pointer :: b1(:,:)

    allocate(Container(4)::c1)
    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1(4,3), SOURCE=reshape((/(Base(4)(i),i=1,12)/),(/4,3/)))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base(4))
            print *, "func1 Base", b
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1(2,5), SOURCE=reshape((/(Child(4)(-i,i),i=1,10)/),(/2,5/)))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Child(4))
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
