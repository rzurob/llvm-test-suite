! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2007
!*
!*  DESCRIPTION                : miscellanous (defect 328685.test)
!                               use xlf90/xlf95 to compile
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, allocatable :: data
    end type

    contains

    function genBaseArray ()
      type(base) genBaseArray (2)
    end function
end module

program d325889
use m
    type(base) :: b2(2)


    b2 = genBaseArray ()

end

