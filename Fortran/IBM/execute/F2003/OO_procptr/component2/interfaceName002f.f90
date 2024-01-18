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
!                              pointer and are arrays.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

module m2
use m1
    contains

    subroutine sub1(b)
        class(Base), pointer, intent(in) :: b(:,:)
        select type (b)
            type is (Base)
                print *, "sub1 Base", b
            type is (Child)
                print *, "sub1 Child", b
            class default
                error stop 3_4
        end select
    end subroutine

    function func1(b)
        class(Base), pointer, intent(in) :: b(:,:)
        class(Base), allocatable :: func1
        select type (b)
            type is (Base)
                allocate(func1, SOURCE=Base(sum(b%i)))
            type is (Child)
                allocate(func1, SOURCE=Child(sum(b%i),sum(b%j)))
            class default
                error stop 4_4
        end select
    end function
end module

module m3
use m2
    type Container
        procedure(sub1), pointer, nopass :: pp1
        procedure(func1), pointer, nopass :: pp2
    end type
end module

program interfaceName002f
use m3
    implicit type(Container) (c)

    class(Base), pointer :: b1(:,:)

    c1%pp1 => sub1
    c1%pp2 => func1

    allocate(b1(4,3), SOURCE=reshape((/(Base(i),i=1,12)/),(/4,3/)))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Base)
            print *, "func1 Base", b
        class default
            error stop 1_4
    end select

    deallocate(b1)

    allocate(b1(2,5), SOURCE=reshape((/(Child(-i,i),i=1,10)/),(/2,5/)))
    call c1%pp1(b1)
    select type (b=>c1%pp2(b1))
        type is (Child)
            print *, "func1 Child", b
        class default
            error stop 2_4
    end select
end
