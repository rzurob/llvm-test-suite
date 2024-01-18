! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/reshape/functionReturn007.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn007.f
! %VERIFY: functionReturn007.out:functionReturn007.vf
! %STDIN:
! %STDOUT: functionReturn007.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Cross testing with finalization.
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
        integer(k1)   :: i = 1
        contains
        final :: finalizeBase, finalizeRank1, finalizeRank2
    end type

    contains

    subroutine finalizeBase(b)
        type(Base(*,4)), intent(inout) :: b
        print *, "Base"
    end subroutine

    subroutine finalizeRank1(b)
        type(Base(*,4)), intent(inout) :: b(:)
        print *, "X"
    end subroutine

    subroutine finalizeRank2(b)
        type(Base(*,4)), intent(inout) :: b(:,:)
        print *, "Y"
    end subroutine
end module

program functionReturn007
use m
    type(Base(20,4)) :: b1(16)
    b1%i = (/ (i, i=1,16) /)

    print *, reshape(b1, (/3,5/))
end
