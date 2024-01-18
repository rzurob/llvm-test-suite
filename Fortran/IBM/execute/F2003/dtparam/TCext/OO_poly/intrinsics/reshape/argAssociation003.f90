! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/argAssociation003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 11/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
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

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(*) :: arg1(:)
        class(*) :: arg2(20)
        class(*) :: arg3(10)
        class(*) :: arg4(:)
        class(*), allocatable :: p1(:)
        class(*), allocatable :: p2(:)

        allocate(p1(2), SOURCE=(/Base(4,20)(-1),Base(4,20)(-2)/))
        allocate(p2(2), SOURCE=(/Child(4,20)(-1,1),Child(4,20)(-2,2)/))

        select type(name1=>reshape(arg1, (/3,5/), p1, (/2,1/)))
            type is (Base(4,*))
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name2=>reshape(arg2, (/3,5/), p2, (/2,1/)))
            type is (Child(4,*))
                print *, name2
            class default
                error stop 2_4
        end select

        select type(name3=>reshape(arg3, (/3,5/), p1, (/2,1/)))
            type is (Base(4,*))
                print *, name3
            class default
                error stop 3_4
        end select

        select type(name4=>reshape(arg4, (/3,5/), p2, (/2,1/)))
            type is (Child(4,*))
                print *, name4
            class default
                error stop 4_4
        end select

    end subroutine
end module

program argAssociation003
use m
    class(Base(4,:)), allocatable :: ap1(:)
    class(Child(4,:)), pointer :: b2(:) => null()
    type(Base(4,20)) :: b1(10)
    type(Child(4,20)) :: c1(20)

    allocate(ap1(10), SOURCE=(/ (Base(4,20)(i), i=1,10) /))
    allocate(b2(20), SOURCE=(/ (Child(4,20)(i,i+100), i=1,20) /))
    b1 = (/ (Base(4,20)(i), i=-2,-20,-2) /)
    c1 = (/ (Child(4,20)(i,i-1), i=31,50) /)

    call sub1(ap1, b2, b1, c1)
end
