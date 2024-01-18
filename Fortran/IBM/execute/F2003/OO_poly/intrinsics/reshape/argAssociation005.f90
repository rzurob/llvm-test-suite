! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation005.f
! %VERIFY: argAssociation005.out:argAssociation005.vf
! %STDIN:
! %STDOUT: argAssociation005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/22/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE of RESHAPE is a dummy argument.
!*    Dummy argument is a pointer or allocatable, and poly.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation005
use m
    class(AbstractParent), pointer :: b1(:)
    class(Base), allocatable :: b2(:,:)
    class(Child), pointer :: c1(:,:)
    class(AbstractParent), allocatable :: c2(:,:)

    allocate(b1(10), SOURCE=(/ (Base(i), i=-2,-20,-2) /))
    allocate(b2(4,5), SOURCE=reshape((/(Base(i), i=1,20)/),(/4,5/)))
    allocate(c1(2,5), SOURCE=reshape((/(Child(i,i-1), i=31,40)/), &
     (/2,5/)))
    allocate(c2(3,6), SOURCE=reshape((/(Child(i,i+100), i=1,20)/), &
     (/3,6/), (/Child(-1,1)/), (/2,1/)))

    call sub1(b1, b2, c1, c2)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(AbstractParent), pointer :: arg1(:)
        class(Base), allocatable :: arg2(:,:)
        class(Child), pointer :: arg3(:,:)
        class(AbstractParent), allocatable :: arg4(:,:)

        select type (name1=>reshape(arg1, (/3,5/), &
         (/Base(-1),Base(-2)/), (/2,1/)))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        select type (name1=>reshape(arg2, (/3,5/), &
         (/Base(-1),Base(-2)/), (/2,1/)))
            type is (Base)
                print *, name1
            class default
                error stop 2_4
        end select

        select type (name1=>reshape(arg3, (/3,2,3/), &
         (/Child(-1,1),Child(-2,2)/), (/3,2,1/)))
            type is (Child)
                print *, name1
            class default
                error stop 3_4
        end select

        select type (name1=>reshape(arg4, (/9/), &
         (/Child(-1,1),Child(-2,2)/), (/1/)))
            type is (Child)
                print *, name1
            class default
                error stop 4_4
        end select
    end subroutine
end
