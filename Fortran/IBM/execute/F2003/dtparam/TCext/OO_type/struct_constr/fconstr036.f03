! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr036.f
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
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type
end module

module m1
use m
    type, extends (dataType) :: mData(k2)    ! (4,20,4)
        integer, kind :: k2
        integer(k2)   :: id
    end type

    type container(k3,n2)    ! (4,20)
        integer, kind                      :: k3
        integer, len                       :: n2
        type(mData(k3,n2,k3)), allocatable :: data(:)
    end type
end module

program fconstr036
use m1


    type (mData(4,20,4)), allocatable :: d1(:)

    type (container(4,20)) :: c1

    c1 = container(4,20) (data = d1)

    if (allocated (c1%data)) error stop 1_4

    allocate (d1(-2:5))

    d1 = (/(mData(4,20,4) (id = 10*i), i=1,8)/)

    c1 = container(4,20) (data = d1)

    if ((size(c1%data) /= 8) .or. (lbound(c1%data, 1) /= -2) .or.  &
        (ubound(c1%data, 1) /= 5)) error stop 2_4

    do i = 1, 8
        if (c1%data(i-3)%id /= i*10) error stop 3_4
    end do
end
