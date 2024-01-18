! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transpose/argAssociation001.f
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, and non-poly.
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
    type(Base(20,4)) :: b(3,4)
    class(Base(:,4)), pointer :: c(:,:)

    b = reshape((/(Base(20,4)(i),i=1,12)/), (/3,4/))
    allocate(c(4,2), SOURCE=reshape((/(Child(20,4)(i,i-1),i=2,9)/), (/4,2/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(*,4)) :: arg1(3, 4)
        type(Base(*,4)) :: arg2(:,:)

        print *, transpose(arg1)
        print *, transpose(arg2)
    end subroutine
end
