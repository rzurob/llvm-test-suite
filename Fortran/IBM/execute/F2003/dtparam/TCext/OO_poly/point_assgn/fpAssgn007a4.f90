! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn007a4.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer
!*                               deallocated; then the dynamic type is declared
!*                               type)
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
        contains

        procedure, nopass :: typeID => baseTypeID
    end type

    type, extends (base) :: child    ! (4)
        integer(k1) :: id

        contains

        procedure, nopass :: typeID => childTypeID
    end type

    contains

    integer*4 function baseTypeID ()
        baseTypeID = 1
    end function

    integer*4 function childTypeID ()
        childTypeID = 2
    end function
end module

program fpAssgn007a4
use m
    class (base(4)), pointer :: b_ptr

    class (child(4)), pointer :: c1
    type (child(4)), pointer :: c2

    b_ptr => null()
    nullify (c1)

    c2 => null()

    if ((b_ptr%typeID() /= 1) .or. (c1%typeID() /= 2) .or. &
        (c2%typeID() /= 2)) error stop 1_4

    allocate (c1)

    b_ptr => c1

    if (b_ptr%typeID() /= 2) error stop 2_4

    deallocate (b_ptr)

    if (b_ptr%typeID() /= 1) error stop 3_4

    allocate (c2)

    b_ptr => c2

    if (b_ptr%typeID() /= 2) error stop 4_4

    deallocate (b_ptr)

    if (b_ptr%typeID() /= 1) error stop 5_4
end
