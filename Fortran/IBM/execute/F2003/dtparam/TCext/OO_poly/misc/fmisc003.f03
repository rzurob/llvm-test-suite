! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 273989)
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
    type base (k1)
        integer, kind :: k1
        integer(k1) :: id = 0
    end type
end module

program fmisc003
use m

    integer, parameter :: k = kind(1)

    type(base(k)) :: b1(2), b2
    type(base(k)), pointer :: b_ptr

    b1 = (/createBase(10), createBase(-10)/)

    b2 = createBase(20)
    print *, b2

    allocate (b_ptr)
    b_ptr = base(k)(20)

    b2 = b_ptr

    print *, b2

    print *, b1

    contains

    function createBase(i)
        integer*4, intent(in) :: i
        type(base(i%kind)), pointer :: createBase

        allocate (createBase)

        createBase = base(i%kind)(i)
    end function
end
