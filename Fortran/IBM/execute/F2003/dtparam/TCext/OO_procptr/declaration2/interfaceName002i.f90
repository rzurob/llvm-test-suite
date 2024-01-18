! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_procptr/declaration2/interfaceName002i.f
! opt variations: -qnok -ql -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare a procedure pointer using
!                              procedure declaration statement. Specify
!                              procedure interface using interface-name,
!                              which is either a module subroutine or
!                              a module function. Poly. Intrinsic
!                              or derived type, scalar or array.
!
!                              This test case use explicit interface
!                              implied by use association to declare the
!                              interface-name before calling module
!                              subroutine and function. The actual
!                              procedure associated has the same name as
!                              interface-name. The dummy arguments of
!                              the associated procedure are arrays. This
!                              test case involves sequence association
!                              where the size and shape of the actual
!                              argument do not match those of the
!                              dummy argument.
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
        class(AbstractParent(4)), intent(in) :: b(2,4)
        select type (b)
            type is (Base(4))
                print *, "sub1 Base", b
            type is (Child(4))
                print *, "sub1 Child", b
            class default
                error stop 1_4
        end select
    end subroutine

    integer function func1(b)
        class(AbstractParent(4)) :: b(3:*)
        select type (b)
            type is (Base(4))
                print *, "func1", b(:8)
                func1 = size(b(:8))
            class default
                error stop 2_4
        end select
    end function

    type(Base(4)) function func2(b)
        class(*) :: b(5,3)
        select type (b)
            type is (Child(4))
                print *, "func2", b
                func2 = Base(4)(size(b))
            class default
                error stop 3_4
        end select
    end function
end module

program interfaceName002i
use m
    procedure(sub1), pointer :: pp1
    procedure(func1), pointer :: pp2
    procedure(func2), pointer :: pp3

    integer rv
    class(AbstractParent(4)), pointer :: b1(:,:)
    class(*), allocatable :: b2(:,:,:)

    pp1 => sub1
    pp2 => func1
    pp3 => func2

    call pp1(reshape((/(Base(4)(i),i=1,18)/),(/3,3,3/), &
     (/Base(4)(-1),Base(4)(-2)/),(/2,3,1/)))

    allocate(b1(3,2), SOURCE=reshape((/(Base(4)(i),i=1,6)/),(/3,2/), &
     (/Base(4)(-1)/),(/2,1/)))
    rv = pp2(b1)
    print *, "Func1", rv

    allocate(b2(4,3,2), SOURCE=reshape((/(Child(4)(i,i+2),i=11,25)/), &
     (/4,3,2/), (/Child(4)(-1,-3),Child(4)(-2,-4)/), (/2,3,1/)))
    associate(name1=>pp3(b2))
        print *, "Func2", name1
    end associate
end
