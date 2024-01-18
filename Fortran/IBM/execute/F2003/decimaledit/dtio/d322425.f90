!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 322425)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        class(*), allocatable :: x(:)
    end type

    type base
        class (dataType), allocatable :: data
    end type
end module

program dcmlChildWrite005
use m
    type(base), allocatable :: b1(:)

    allocate (b1(12))

    b1(3) = base(dataType((/base(dataType(null()))/)))

    select type (x => b1(3)%data%x)
        type is (base)
            if (size(x) /= 1) stop 1

            if (allocated(x(1)%data%x)) stop 2
        class default
            stop 10
    end select
end
