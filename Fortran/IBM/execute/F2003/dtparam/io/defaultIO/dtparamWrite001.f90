! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/14/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               default I/O on derived type with type
!                               parameters: list-directed write
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point
        integer x, y
    end type

    type base (n)
        integer, len :: n

        type(point) :: data(n)
    end type
end module

program dtparamWrite001
use m
    type (base(:)), allocatable :: b1(:)

    b1 = (/(base(5)(data=(/(point(x=j,y=j), j=i-4,i)/)), i=1,8)/)

    if (.not. allocated(b1)) error stop 1_4

    if ((lbound(b1,1) /= 1) .or. (ubound(b1,1) /= 8)) error stop 2_4

    print *, b1

    do i = 1, 8
        print *, b1(i)
        print *, b1(i)%data
    end do
end
