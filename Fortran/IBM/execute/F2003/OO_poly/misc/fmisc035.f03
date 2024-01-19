! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 284854)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: id
    end type

    contains

    elemental logical function baseEqual (b1, b2)
        type (base), intent (in) :: b1, b2

        baseEqual = (b1%id == b2%id)
    end function
end module

program fmisc035
use m
    type (base) :: b1(10)

    b1%id = (/(i,i=1,10)/)

    forall (i=1:10, baseEqual (b1(i), base(i)))
        b1(i) = base(-i)
    end forall

    print *, b1
end

