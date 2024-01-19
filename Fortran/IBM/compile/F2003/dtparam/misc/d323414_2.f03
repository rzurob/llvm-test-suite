! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/13/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 323414.2)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        real(8), allocatable :: data(:)
    end type

    type base
        type(dataType), allocatable :: data(:)
    end type

end module

use m
    type(base), allocatable :: b1(:)

    i = 10
    j = 10
    allocate (b1(10))

    allocate (b1(10)%data(10))

    print *, allocated (b1(i)%data(j)) !<-- illegal
end

