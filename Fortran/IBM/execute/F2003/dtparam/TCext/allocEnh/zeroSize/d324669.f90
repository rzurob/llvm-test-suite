! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/d324669.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
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
    type base(n1,k1)    ! (20,8)
        integer, kind              :: k1
        integer, len               :: n1
        real(k1), pointer, private :: data(:) => null()

        contains

        procedure :: print => printBase
    end type

    interface base
        module procedure genBaseObj
    end interface

    contains

    type(base(20,8)) function genBaseObj (r1)
        real(8), intent(in) :: r1(:)

        allocate (genBaseObj%data(size(r1)), source=r1)
    end function

    subroutine printBase (b)
        class(base(*,8)), intent(in) :: b

        if (associated(b%data)) write (*, '(10g12.6)') b%data
    end subroutine
end module

program zeroSizeArray009
use m
    type (base(:,8)), allocatable :: b1(:)

    allocate (base(20,8) :: b1(3))

    b1 = (/(base((/(j*1.0d0, j=1,i)/)), i=1,3)/)

    do i = 1, 3
        call b1(i)%print
    end do
end
