! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_tpbnd/specific/ftpbnd518a1_1.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2005
!*
!*  DESCRIPTION                : specific type bound (type bound is used in the
!                               specification expression)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        class(*), allocatable :: data(:)

        contains

        procedure :: lbound => lowerBound
        procedure :: ubound => upperBound
    end type

    contains

    pure integer function lowerBound (b)
        class (base(4)), intent(in) :: b

        if (allocated (b%data)) then
            lowerBound = lbound(b%data, 1)
        else
            lowerBound = 1
        end if
    end function

    pure integer function  upperBound(b)
        class (base(4)), intent(in) :: b

        if (allocated (b%data)) then
            upperBound = ubound(b%data, 1)
        else
            upperBound = 0
        end if
    end function
end module


program ftpbnd518a1_1
use m
    type (base(4)), pointer :: b1
    integer i1 (100)

    allocate (b1)
    allocate (b1%data(0:99), source=(/(j, j=1, size(i1))/))

    call scalarize (b1, i1)

    if (any (i1 /= (/(j, j=1, 100)/))) error stop 2_4

    contains

    subroutine scalarize (b1, x1)
        class (base(4)), intent(in) :: b1
        integer, intent(out) :: x1(b1%lbound():b1%ubound())

        do i = lbound(x1, 1), ubound(x1, 1)
            select type (x =>b1%data(i))
                type is (integer)
                    x1(i) = x
                class default
                    error stop 1_4
            end select
        end do
    end subroutine
end
