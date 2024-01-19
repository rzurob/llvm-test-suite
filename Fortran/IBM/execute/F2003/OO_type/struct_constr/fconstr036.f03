! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructure (allocatable component
!*                               initialized in the structure constructor; if
!*                               the expression is an allocatable entity, the
!*                               component will have the same allocation status
!*                               if it is allocataed, the same dynamic type,
!*                               bounds and values)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
    end type
end module

module m1
use m
    type, extends (dataType) :: mData
        integer*4 :: id
    end type

    type container
        type (mData), allocatable :: data(:)
    end type
end module

program fconstr036
use m1


    type (mData), allocatable :: d1(:)

    type (container) :: c1

    c1 = container (data = d1)

    if (allocated (c1%data)) error stop 1_4

    allocate (d1(-2:5))

    d1 = (/(mData (id = 10*i), i=1,8)/)

    c1 = container (data = d1)

    if ((size(c1%data) /= 8) .or. (lbound(c1%data, 1) /= -2) .or.  &
        (ubound(c1%data, 1) /= 5)) error stop 2_4

    do i = 1, 8
        if (c1%data(i-3)%id /= i*10) error stop 3_4
    end do
end
