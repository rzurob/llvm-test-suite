! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn501.f
! opt variations: -qck -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited-poly data
!                               used as SOURCE in transfer())
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

    type, extends(base) :: child(n1)    ! (4,15)
        integer, len  :: n1
        character(n1) :: name
    end type
end module

program fpAssgn501
use m
    class (*), pointer :: x (:)

    type (child(4,15)), target :: c1(2:3), c2(2)

    c1%id = (/1,2/)
    c1%name = (/'c1_1', 'c1_2'/)

    x => c1

    c2 = transfer (x, c1,2)

    if (any (c2%id /= c1%id)) error stop 1_4

    if (any (c2%name /= c1%name)) error stop 2_4
end
