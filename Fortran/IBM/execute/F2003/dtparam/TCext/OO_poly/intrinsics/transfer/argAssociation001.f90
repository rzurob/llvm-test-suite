! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/argAssociation001.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, non-poly, and is scalar.
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

    type Base1(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        integer(k2)          j
        type(Base(n2,k2)) :: k
        integer(k2)          m
    end type

    type, extends(Base1) :: Child1    ! (4,20)
        integer(k2) n
    end type
end module

program argAssociation001
use m
    type(Base(20,4)) :: b
    type(Base1(4,20)) :: b1
    class(Base(:,4)), pointer :: c
    class(Base1(4,:)), allocatable :: c1

    b = Base(20,4)(10)
    b1 = Base1(4,20)(7, Base(20,4)(8), 9)
    allocate(c, SOURCE=Child(20,4)(3,4))
    allocate(c1, SOURCE=Child1(4,20)(m=14,n=15,k=Base(20,4)(13),j=12))

    call sub1(b, b1, c, c1)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        type(Base(*,4)) :: arg1
        type(Base1(4,*)) :: arg2
        type(Base(*,4)) :: arg3
        type(Base1(4,*)) :: arg4

        print *, transfer(arg2, arg1, 3)
        print *, transfer(arg4, arg3, 3)
    end subroutine
end
