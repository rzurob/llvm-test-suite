!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr036a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
        class (dataType), allocatable :: data(:)
    end type
end module

program fconstr036a
use m1


    type (mData), allocatable :: d1(:)
    type (dataType), allocatable :: d2(:)

    type (container) :: c1, c2

    c1 = container (data = d1)
    c2 = container (data = d2)

    if (allocated (c1%data) .or. allocated (c2%data)) error stop 1_4

    allocate (d1(-2:5), d2(-10:0))

    d1 = (/(mData (id = 10*i), i=1,8)/)

    d2 = (/(dataType(), i=1, 11)/)

    c1 = container (data = d1)
    c2 = container (data = d2)

    if ((size(c1%data) /= 8) .or. (lbound(c1%data, 1) /= -2) .or.  &
        (ubound(c1%data, 1) /= 5)) error stop 2_4

    if ((size(c2%data) /= 11) .or. (lbound(c2%data, 1) /= -10) .or. &
        (ubound(c2%data, 1) /= 0)) error stop 3_4

    !! very the data type of the component
    select type (x => c1%data)
        type is (mData)
            if (any (x%id /= (/(j, j=10,80,10)/))) error stop 5_4
        class default
            error stop 10_4
    end select


    select type (x => c2%data)
        type is (dataType)
        class default
            error stop 12_4
    end select
end
