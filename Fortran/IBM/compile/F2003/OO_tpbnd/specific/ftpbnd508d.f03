! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : specific type bound (binding name appears in
!                               the data statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) id

        contains

        procedure :: value => baseID
    end type

    type (base) b1_m

    data b1_m%id, b1_m%value  /1, baseID(base(10))/     !<-- illegal syntax

    contains

    integer(4) function baseID(b)
        class (base), intent(in) :: b

        baseID = b%id
    end function
end module


program ftpbnd508d
end