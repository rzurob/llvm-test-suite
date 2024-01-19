! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/misc/d328685_test.f
! opt variations: -ql

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
    type base(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data
    end type

    contains

    function genBaseArray ()
      type(base(4)) genBaseArray (2)
    end function
end module

program d325889
use m
    type(base(4)) :: b2(2)


    b2 = genBaseArray ()

end

