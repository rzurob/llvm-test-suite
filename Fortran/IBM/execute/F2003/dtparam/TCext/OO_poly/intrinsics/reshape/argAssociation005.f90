! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/argAssociation005.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
end module

program argAssociation005
use m
    class(AbstractParent(4,:)), pointer :: b1(:)
    class(Base(4,:)), allocatable :: b2(:,:)
    class(Child(4,:)), pointer :: c1(:,:)
    class(AbstractParent(4,:)), allocatable :: c2(:,:)

    allocate(b1(10), SOURCE=(/ (Base(4,20)(i), i=-2,-20,-2) /))
    allocate(b2(4,5), SOURCE=reshape((/(Base(4,20)(i), i=1,20)/),(/4,5/)))
    allocate(c1(2,5), SOURCE=reshape((/(Child(4,20)(i,i-1), i=31,40)/), &
     (/2,5/)))
    allocate(c2(3,6), SOURCE=reshape((/(Child(4,20)(i,i+100), i=1,20)/), &
     (/3,6/), (/Child(4,20)(-1,1)/), (/2,1/)))

    call sub1(b1, b2, c1, c2)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(AbstractParent(4,:)), pointer :: arg1(:)
        class(Base(4,:)), allocatable :: arg2(:,:)
        class(Child(4,:)), pointer :: arg3(:,:)
        class(AbstractParent(4,:)), allocatable :: arg4(:,:)

        select type (name1=>reshape(arg1, (/3,5/), &
         (/Base(4,20)(-1),Base(4,20)(-2)/), (/2,1/)))
            type is (Base(4,*))
                print *, name1
            class default
                error stop 1_4
        end select

        select type (name1=>reshape(arg2, (/3,5/), &
         (/Base(4,20)(-1),Base(4,20)(-2)/), (/2,1/)))
            type is (Base(4,*))
                print *, name1
            class default
                error stop 2_4
        end select

        select type (name1=>reshape(arg3, (/3,2,3/), &
         (/Child(4,20)(-1,1),Child(4,20)(-2,2)/), (/3,2,1/)))
            type is (Child(4,*))
                print *, name1
            class default
                error stop 3_4
        end select

        select type (name1=>reshape(arg4, (/9/), &
         (/Child(4,20)(-1,1),Child(4,20)(-2,2)/), (/1/)))
            type is (Child(4,*))
                print *, name1
            class default
                error stop 4_4
        end select
    end subroutine
end
