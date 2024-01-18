! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transpose/diagnose005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Diagnose test case.
!*    When MATRIX is poly, the return value of transpose shall be
!*  poly and thus shall not processed by regular IO.
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

program diagnose005
use m
    class(Base(:,4)), pointer :: b2(:,:)

    allocate(b2(4,4), SOURCE=reshape((/(Child(20,4)(i,i+1),i=5,20)/),(/4,4/)))

    print *, transpose(b2)
end
