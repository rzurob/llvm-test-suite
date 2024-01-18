! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd521d1.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (non-pure procedure cannot
!                               be used in elemental function)
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

        procedure :: getID => getBaseID
    end type

    contains

    integer(4) function getBaseID (b)
        class (base(*,4)), intent(in) :: b

        getBaseID = b%id
    end function

    elemental logical function find1 (b, id)
        class (base(*,4)), intent(in) :: b
        integer(4), intent(in) :: id

        find1 = (b%getID() == id)
    end function
end module

program ftpbnd521d1
end
