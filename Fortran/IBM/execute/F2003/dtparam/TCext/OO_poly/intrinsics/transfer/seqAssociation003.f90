! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self /tstdev/OO_poly/intrinsics/transfer/seqAssociation003.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: seqAssociation003.f
! %VERIFY: seqAssociation003.out:seqAssociation003.vf
! %STDIN:
! %STDOUT: seqAssociation003.out
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
!*  is an explicit-shape or assumed-size array. Actual argument is
!*  sequence associated with dummy argument. Actual argument is an
!*  array element. Non-poly.
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

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       j
        type(Base(k2)) :: k
        integer(k2)       m
    end type
end module

program seqAssociation003
use m
    type(Base(4)) :: b(40)
    type(Base1(4)) :: b1(4,3)

    b = (/(Base(4)(i),i=1,40)/)
    b1 = reshape((/(Base1(4)(i, Base(4)(i+1), i+2),i=3,18)/), (/4,3/))

    call sub1(b(2), b1(3,2))

    contains

    subroutine sub1(arg1, arg2)
        type(Base(4)) :: arg1(9)
        type(Base1(4)) :: arg2(*)

        print *, transfer(arg1, arg2(:2))
        print *, transfer(arg2(2:5), arg1, 12)
    end subroutine
end
