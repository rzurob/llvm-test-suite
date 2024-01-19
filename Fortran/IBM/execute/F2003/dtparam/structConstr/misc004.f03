! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2006
!*
!*  DESCRIPTION                : miscellaneous
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real :: data(10)

        contains

        procedure, nopass :: gen => genBaseObj
        procedure :: val => returnBaseData
    end type

    interface base
        module procedure genBaseObj
    end interface

    contains

    type(base) function genBaseObj (val)
        real, intent(in) :: val(10)

        genBaseObj%data = val
    end function

    real function returnBaseData (b)
        class(base), intent(inout) :: b

        dimension returnBaseData(10)

        returnBaseData = b%data

        b%data = b%data+1.0
    end function
end module

program misc004
use m
    type(base) b1, b2

    logical(4), external ::precision_r4

    !! the following reference to the structure constructor is legal
    b1 = base(data=(/(i*1.0, i=1,10)/))

    b2 = b1%gen(b1%val()*b1%val()+1.2e1)

    !! verify
    do i = 1,10
        if (.not. precision_r4(b2%data(i), i*(i+1)+1.2e1)) error stop 1_4

        if (.not. precision_r4(b1%data(i), (i+2)*1.0)) error stop 2_4
    end do
end
