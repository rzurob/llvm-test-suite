! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd502a1.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (basic use of elemental
!*                               function as type-bound)
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

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure, non_overridable :: getID => baseID
    end type

    type (base(20,4)) :: b1_m(10), b2_m

    contains

    elemental integer*4 function baseID (b)
        class (base(*,4)), intent(in) :: b

        baseID = b%id
    end function
end module

program ftpbnd502a1
use m
    type (base(:,4)), pointer :: b1(:)
    integer*4 :: ret(10)

    b2_m = base(20,4)(1)
    b1_m(:) = (/(base(20,4)(i), i=2,11)/)

    if (b2_m%getID() /= 1) error stop 1_4

    if (b1_m(10)%getID() /= 11) error stop 2_4

    ret = b1_m%getID()

    if (any (ret /= (/(i, i=2,11)/))) error stop 3_4

    allocate (base(20,4) :: b1(10))

    b1 = (/(base(20,4)(-i),i=1,10)/)

    if (b1(3)%getID() /= -3) error stop 5_4

    ret = b1%getID()

    if (any (ret /= b1%id)) error stop 6_4

    deallocate (b1)
end
