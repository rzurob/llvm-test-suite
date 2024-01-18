!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/22/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 328616)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data
    end type

    contains

    function genBase (b, b2)
        class(base), intent(in) :: b(:,:), b2(:,:)

        type(base) genBase(size(b,1),size(b,2))

        do i = 1, size(b,1)
            do j = 1, size(b,2)
                genBase(i,j) = base(b2(i,j)%data)
            end do
        end do
    end function
end module

use m
    class(base), allocatable :: b1(:,:)
    type(base), allocatable :: b2, b3(:,:)

    b2 = base(1)

    allocate (b1(3,3), source=reshape((/(base(i*1.2), i=1,3), &
        (base(i), i=1,3), (base(i*1_8), i=1,3)/), (/3,3/)))

    associate (x => genBase(reshape((/(b2, i=1,9)/),(/3,3/)), b1), &
        y => genBase(b1,reshape((/(b2, i=1,9)/),(/3,3/))))
    end associate
end
