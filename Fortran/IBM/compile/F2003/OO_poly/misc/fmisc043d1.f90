!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/24/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 311858)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real, target, save, private :: r1 = -1.0

    contains

    real function t1()
        pointer t1

        r1 = -1.0
        t1 => r1
    end function
end module

program fmisc043d1
use m
    procedure(t1) :: ptr    !<-- missing pointer attribute for ptr

    ptr => t1               !<-- illegal

end
