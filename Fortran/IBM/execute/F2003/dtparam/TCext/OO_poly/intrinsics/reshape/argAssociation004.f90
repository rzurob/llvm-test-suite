! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/argAssociation004.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation004.f
! %VERIFY: argAssociation004.out:argAssociation004.vf
! %STDIN:
! %STDOUT: argAssociation004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/22/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE of RESHAPE is a dummy argument.
!*    Dummy argument is a pointer or allocatable, and non-poly.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program argAssociation004
use m
    type(Base(4)), pointer :: b1(:)
    type(Base(4)), allocatable :: b2(:,:)
    type(Child(4)), pointer :: c1(:,:)
    type(Child(4)), allocatable :: c2(:,:)

    allocate(b1(10), SOURCE=(/ (Base(4)(i), i=-2,-20,-2) /))
    allocate(b2(4,5), SOURCE=reshape((/(Base(4)(i), i=1,20)/),(/4,5/)))
    allocate(c1(2,5), SOURCE=reshape((/(Child(4)(i,i-1), i=31,40)/), &
     (/2,5/)))
    allocate(c2(3,6), SOURCE=reshape((/(Child(4)(i,i+100), i=1,20)/), &
     (/3,6/), (/Child(4)(-1,1)/), (/2,1/)))

    call sub1(b1, b2, c1, c2)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base(4)), pointer :: arg1(:)
        type(Base(4)), allocatable :: arg2(:,:)
        type(Child(4)), pointer :: arg3(:,:)
        type(Child(4)), allocatable :: arg4(:,:)

        print *, reshape(arg1, (/3,5/), (/Base(4)(-1),Base(4)(-2)/), (/2,1/))
        print *, reshape(arg2, (/3,5/), (/Base(4)(-1),Base(4)(-2)/), (/2,1/))
        print *, reshape(arg3, (/3,2,3/), (/Child(4)(-1,1),Child(4)(-2,2)/), &
         (/3,2,1/))
        print *, reshape(arg4, (/9/), (/Child(4)(-1,1),Child(4)(-2,2)/), &
         (/1/))
    end subroutine
end
