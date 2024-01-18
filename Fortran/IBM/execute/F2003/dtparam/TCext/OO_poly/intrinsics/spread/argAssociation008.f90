! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/spread/argAssociation008.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation008.f
! %VERIFY: argAssociation008.out:argAssociation008.vf
! %STDIN:
! %STDOUT: argAssociation008.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/20/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is a pointer or
!*  allocatable, non-poly, and is array.
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

program argAssociation008
use m
    type(Base(4)), pointer :: b(:)
    type(Child(4)), allocatable :: c(:,:)

    allocate(b(10), SOURCE=(/(Base(4)(i),i=1,10)/))
    allocate(c(2,3), SOURCE=reshape((/(Child(4)(i,-i),i=3,8)/), &
     (/2,3/), (/Child(4)(-1,-2)/), (/2,1/)))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(4)), pointer :: arg1(:)
        type(Child(4)), allocatable :: arg2(:,:)

        print *, spread(arg1, 2, 3)
        print *, spread(arg2, 3, 2)
    end subroutine
end
