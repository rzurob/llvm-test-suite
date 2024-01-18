! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: seqAssociation001.f
! %VERIFY: seqAssociation001.out:seqAssociation001.vf
! %STDIN:
! %STDOUT: seqAssociation001.out
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
!*  is an explicit-shape or assumed-size array. Actual argument is
!*  sequence associated with dummy argument. Actual argument is an
!*  array section. Non-poly.
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
    type Base
        integer i
    end type

    type Base1
        integer j
        type(Base) :: k
        integer m
    end type
end module

program seqAssociation001
use m
    type(Base) :: b(40)
    type(Base1) :: b1(4,3)

    b = (/(Base(i),i=1,40)/)
    b1 = reshape((/(Base1(i, Base(i+1), i+2),i=3,18)/), (/4,3/))

    call sub1(b(2:20:2), b1(:3,2:3))

    contains

    subroutine sub1(arg1, arg2)
        type(Base) :: arg1(9)
        type(Base1) :: arg2(*)

        print *, transfer(arg1, arg2(:2))
        print *, transfer(arg2(2:6), arg1, 12)
    end subroutine
end
