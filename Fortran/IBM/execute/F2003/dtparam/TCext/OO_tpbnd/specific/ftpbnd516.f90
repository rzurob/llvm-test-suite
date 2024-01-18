! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd516.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (function that returns
!*                               allocatable array)
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

        contains

        procedure :: makeArray => createBaseArray
    end type

    contains

    function createBaseArray (b, n)
        class (base(4)), intent(in) :: b
        type (base(4)), allocatable :: createBaseArray (:)
        integer*4, intent(in) :: n

        allocate (createBaseArray(n))
        createBaseArray = base(4)(b%id)
    end function
end module

program ftpbnd516
use m
    type (base(4)) :: b1, b2 (10)

    b1%id = 10

    print *, b1%makeArray(2)

    b1%id = 2
    b2 = b1%makeArray(10)

    print *, b2

    if (any (b2%id /= 2)) error stop 1_4
end
