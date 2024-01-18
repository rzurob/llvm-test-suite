! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/05/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 321047)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data
    end type
end module

use m
    class(base), allocatable :: b1(:)

    allocate(b1(30))

    do i = 21, 30
        allocate(b1(i)%data, source=mod(i,2)==0)
    end do

    do i = 1, 10
        select type (x => b1(i+20)%data)
            type is (logical)
                if (x .neqv. mod(i,2)==0) stop 9

            class default
                stop 10
        end select
    end do
end
