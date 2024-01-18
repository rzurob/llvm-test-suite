!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/30/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 324669)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer, private :: data(:) => null()

        contains

        procedure :: print => printBase
    end type

    interface base
        module procedure genBaseObj
    end interface

    contains

    type(base) function genBaseObj (r1)
        real(8), intent(in) :: r1(:)

        allocate (genBaseObj%data(size(r1)), source=r1)
    end function

    subroutine printBase (b)
        class(base), intent(in) :: b

        if (associated(b%data)) write (*, '(10g12.6)') b%data
    end subroutine
end module

program zeroSizeArray009
use m
    type (base), allocatable :: b1(:)

    allocate (b1(3))

    b1 = (/(base((/(j*1.0d0, j=1,i)/)), i=1,3)/)

    do i = 1, 3
        call b1(i)%print
    end do
end
