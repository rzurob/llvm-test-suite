! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/30/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 336286)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex, allocatable :: data(:)
    end type
end module

use m
    type(base) b1

    b1 = base([complex(8) :: ])

    if (.not. allocated(b1%data)) error stop 1_4

    if (size(b1%data) /= 0) error stop 2_4
end