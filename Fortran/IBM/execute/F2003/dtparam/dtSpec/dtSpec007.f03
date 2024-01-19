! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The assumed-type-parameter for dummy-arg,
!                               nonpointer, nonallocatable dummy-arg, scalar
!                               case.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type content (l)
        integer, len :: l

        real :: data
        character(l) :: desc
    end type

    type base (n, l)
        integer, len :: n, l

        real :: data(n) = -1.0
        character(l) :: desc(n) = 'default'

        contains

        procedure :: at => contentAtIndex
    end type

    contains

    function contentAtIndex (b, i)
        class(base(l=*, n=*)), intent(in) :: b
        integer, intent(in) :: i

        type (content(b%l)) contentAtIndex

        if ((i < 0) .or. (i > b%n)) error stop 10

        contentAtIndex%data = b%data(i)
        contentAtIndex%desc = b%desc(i)
    end function
end module

program dtSpec007
use m
    class(base(:,l=:)), allocatable :: b1(:)
    logical(4), external :: precision_r4

    character(*), parameter :: descriptions(10)= (/'element: 0', 'element: 1', &
        'element: 2', 'element: 3', 'element: 4', 'element: 5', 'element: 6', &
        'element: 7', 'element: 8', 'element: 9'/)

    allocate (base(n=10, l=20) :: b1(15))

    do i = 1, 15
        b1(i)%data = (/(i*1.0e2+j, j = 1, 10)/)
        b1(i)%desc = (/('element: '//char(ichar('0')+i), i=0,9)/)
    end do

    !! verify using the associate construct
    do i = 1, 15
        do j = 1, 10
            call verifyContent(b1(i)%at(j), i, j)
        end do
    end do

    contains

    subroutine verifyContent(x, i, j)
        type(content(*)), intent(in) :: x

        if (.not. precision_r4(x%data, i*1.0e2+j*1.0)) error stop 1_4

        if (x%desc /= descriptions(j)) error stop 2_4

        if (len(x%desc) /= 20) error stop 3_4
    end subroutine
end
