! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation003.f
! %VERIFY: argAssociation003.out:argAssociation003.vf
! %STDIN:
! %STDOUT: argAssociation003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE of RESHAPE is a dummy argument.
!*    Dummy argument is non-pointer, non-allocatable, and unlimited poly.
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

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(*) :: arg1(:)
        class(*) :: arg2(20)
        class(*) :: arg3(10)
        class(*) :: arg4(:)
        class(*), allocatable :: p1(:)
        class(*), allocatable :: p2(:)

        allocate(p1(2), SOURCE=(/Base(-1),Base(-2)/))
        allocate(p2(2), SOURCE=(/Child(-1,1),Child(-2,2)/))

        select type(name1=>reshape(arg1, (/3,5/), p1, (/2,1/)))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name2=>reshape(arg2, (/3,5/), p2, (/2,1/)))
            type is (Child)
                print *, name2
            class default
                error stop 2_4
        end select

        select type(name3=>reshape(arg3, (/3,5/), p1, (/2,1/)))
            type is (Base)
                print *, name3
            class default
                error stop 3_4
        end select

        select type(name4=>reshape(arg4, (/3,5/), p2, (/2,1/)))
            type is (Child)
                print *, name4
            class default
                error stop 4_4
        end select

    end subroutine
end module

program argAssociation003
use m
    class(Base), allocatable :: ap1(:)
    class(Child), pointer :: b2(:) => null()
    type(Base) :: b1(10)
    type(Child) :: c1(20)

    allocate(ap1(10), SOURCE=(/ (Base(i), i=1,10) /))
    allocate(b2(20), SOURCE=(/ (Child(i,i+100), i=1,20) /))
    b1 = (/ (Base(i), i=-2,-20,-2) /)
    c1 = (/ (Child(i,i-1), i=31,50) /)

    call sub1(ap1, b2, b1, c1)
end
