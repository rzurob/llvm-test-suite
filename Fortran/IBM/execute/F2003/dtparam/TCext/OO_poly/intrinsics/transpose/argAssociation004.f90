! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/transpose/argAssociation004.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is a pointer or
!*  allocatable, and non-poly.
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
    type(Base(4)), pointer :: b1(:,:)
    type(Base(4)), allocatable :: b2(:,:)

    allocate(b1(3,4), SOURCE=reshape((/(Base(4)(i),i=1,12)/),(/3,4/)))
    allocate(b2(2,3), SOURCE=reshape((/(Base(4)(i),i=3,8)/),(/2,3/)))

    call sub1(b1, b2)

    contains

    subroutine sub1(arg1, arg2)
        type(Base(4)), pointer :: arg1(:,:)
        type(Base(4)), allocatable :: arg2(:,:)

        print *, transpose(arg1)
        print *, transpose(arg2)
    end subroutine
end
