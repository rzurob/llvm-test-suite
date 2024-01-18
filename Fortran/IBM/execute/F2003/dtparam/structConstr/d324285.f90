!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 324285)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realArray! (n)
        real(4) :: data(100)
    end type

    type base
        integer id
        type(realArray), allocatable :: data(:)

        contains

        procedure :: getData => getBaseData
    end type

    contains

    type(realArray) function getBaseData (b1)
        class(base), intent(in) :: b1

        allocatable getBaseData(:)

        if (allocated(b1%data)) then
            getBaseData = b1%data
            print *, lbound(getBaseData), ubound(getBaseData)
        end if
    end function
end module

program dtparamConstr055a
use m
    type (base), allocatable :: b1, b2

    logical(4), external :: precision_r4

    allocate (b1)

    b1 = base(100, b1%getData())

    if ((.not. allocated(b1)) .or. allocated(b1%data)) error stop 1_4

    if (b1%id /= 100) error stop 2_4

    allocate (b1%data(-1:2), &
        source=(/(realArray((/(j, j=10**i,10**i+99)/)), i=1,4)/))

    b2 = base(200, b1%getData())

    if (.not. allocated(b2)) error stop 3_4

    if ((.not. allocated(b2%data)) .or. (size(b2%data(1)%data) /=100)) error stop 4_4

    if ((lbound(b2%data,1) /= -1) .or. (ubound(b2%data,1) /= 2)) error stop 5_4

    if (b2%id /= 200) error stop 7_4

    do i = -1, 2
        do j = 1, 100
            if (.not. precision_r4(b2%data(i)%data(j), (10**(i+2)+j-1)*1.0_4))&
                    error stop 6_4
        end do
    end do
end
