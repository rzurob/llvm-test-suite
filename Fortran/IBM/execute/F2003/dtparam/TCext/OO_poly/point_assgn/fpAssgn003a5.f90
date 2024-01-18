! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn003a5.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (test the associated()
!*                               for 0-sized array; if TARGET exist and is
!*                               0-sized, then it returns false)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn003a5

    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        INTEGER(k1)   :: id
    end type

    class (base(:,4)), pointer :: b1(:)

    type(base(20,4)), target :: b2(1)

    b1 => b2(2:)

    if ((size (b1) /= 0) .or. (associated(b1, b2(2:)))) error stop 1_4

    b1 => b2 (1:0)

    if ((size (b1) /= 0) .or. (associated(b1, b2 (1:0)))) error stop 2_4
end
