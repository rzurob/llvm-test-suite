! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr502.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (elemental function used
!                               for the component data source)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type container(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data (:)
    end type

    contains

    elemental type (base(4)) function addTwo (b1, b2)
        class (base(4)), intent(in) :: b1, b2

        addTwo%id = b1%id + b2%id
    end function
end module

program fconstr502
use m
    class (base(4)), allocatable :: b1(:)

    type (container(4)) :: co1

    allocate (b1(10))

    b1%id = (/(i,i=1,10)/)

    co1 = container(4) (addTwo(base(4) (100), b2=b1))

    if (.not. allocated (co1%data)) error stop 1_4

    if (size (co1%data) /= 10) error stop 2_4

    if (any (co1%data%id /= (/(i,i=101,110)/))) error stop 3_4
end
