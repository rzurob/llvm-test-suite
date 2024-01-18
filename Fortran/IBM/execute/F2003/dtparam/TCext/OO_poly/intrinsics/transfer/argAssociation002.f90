! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/argAssociation002.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation002.f
! %VERIFY: argAssociation002.out:argAssociation002.vf
! %STDIN:
! %STDOUT: argAssociation002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, non-poly, and is array.
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

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       j
        type(Base(k2)) :: k
        integer(k2)       m
    end type

    type, extends(Base1) :: Child1    ! (4)
        integer(k2) n
    end type
end module

program argAssociation002
use m
    type(Base(4)) :: b(10)
    type(Base1(4)) :: b1(2,3)
    class(Base(4)), pointer :: c(:)
    class(Base1(4)), allocatable :: c1(:,:)

    b = (/ (Base(4)(i),i=1,10) /)
    b1 = reshape((/(Base1(4)(i, Base(4)(i+1), i+2),i=5,15,2)/),(/2,3/))
    allocate(c(6), SOURCE=(/(Child(4)(i,i+1),i=2,7)/))
    allocate(c1(2,2), SOURCE=reshape((/(Child1(4)(m=i+2,n=i+3, &
     k=Base(4)(i+1),j=i),i=12,15)/), (/2,2/)))

    call sub1(b, b1, c, c1)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base(4)) :: arg1(10)
        type(Base1(4)) :: arg2(:,:)
        type(Base(4)) :: arg3(:)
        type(Base1(4)) :: arg4(2,2)

        print *, transfer(arg3, arg1)
        print *, transfer(arg2, Child(4)(1,2), 5)
        print *, transfer(arg4, (/Child1(4)(1,Base(4)(1),1,1)/))
    end subroutine
end
