! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/argAssociation001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation001.f
! %VERIFY: argAssociation001.out:argAssociation001.vf
! %STDIN:
! %STDOUT: argAssociation001.out
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
!*    Dummy argument is non-pointer, non-allocatable, and non-poly.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program argAssociation001
use m
    type(Base(20,4)) :: b1(10)
    class(Base(:,4)), pointer :: b2(:) => null()
    type(Child(20,4)) :: c1(10)
    class(Child(:,4)), allocatable :: c2(:)

    b1 = (/ (Base(20,4)(i), i=-2,-20,-2) /)
    c1 = (/ (Child(20,4)(i,i-1), i=31,40) /)
    allocate(b2(20), SOURCE=(/ (Child(20,4)(i,i+1), i=1,20) /))
    allocate(c2(20), SOURCE=(/ (Child(20,4)(i,i+100), i=1,20) /))

    call sub1(b1, b2, c1, c2)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base(*,4)) :: arg1(10)
        type(Base(*,4)) :: arg2(20)
        type(Child(*,4)) :: arg3(10)
        type(Child(*,4)) :: arg4(20)

        type(Base(20,4)) :: x1(3,5)
        type(Child(20,4)) :: y1(3,5)

        x1 = reshape(arg1, (/3,5/), (/Base(20,4)(-1),Base(20,4)(-2)/), (/2,1/))
        print *, x1
        x1 = reshape(arg2, (/3,5/))
        print *, x1
        y1 = reshape(arg3, (/3,5/), (/Child(20,4)(-1,1),Child(20,4)(-2,2)/), (/2,1/))
        print *, y1
        y1 = reshape(arg4, (/3,5/), (/Child(20,4)(-1,1),Child(20,4)(-2,2)/), (/2,1/))
        print *, y1
    end subroutine
end
