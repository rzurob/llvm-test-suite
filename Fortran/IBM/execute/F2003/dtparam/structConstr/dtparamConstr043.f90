!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/01/2006
!*
!*  DESCRIPTION                : derived type parameter (structConstr)
!                               Polymorphic pointer to be associated with a
!                               function result that is of poly pointer of
!                               parameterized derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType (k, n)
        integer, len :: n
        integer, kind :: k

        real(k), allocatable :: data(:)
    end type

    contains

    type (dataType(4,n)) function genData4 (n, r)
        integer, intent(in) :: n
        real(4), intent(in) :: r(n)

        pointer genData4

        allocate (genData4)

        genData4 = dataType(4,n)(r)
    end function

    type (dataType(8,n)) function genData8 (n, r)
        integer, intent(in) :: n
        real(8), intent(in) :: r(n)

        pointer genData8

        allocate (genData8)

        genData8 = dataType(8,n)(r)
    end function
end module

module n
use m
    type base (k, n)
        integer, len :: n
        integer, kind :: k

        class(dataType(k,n)), pointer :: data
    end type
end module

program dtparamConstr043
use n
    type(base(4,:)), allocatable :: b1
    type(base(8,:)), allocatable :: b2

    real r1(1000)
    double precision d1(800)

    logical(4), external :: precision_r4, precision_r8

    allocate (base(4,size(r1)) :: b1)
    allocate (base(8,size(d1)) :: b2)

    r1 = sin((/(i*1.0, i=1,size(r1))/))

    d1 = dlog((/(i*1.0d0, i=1, size(d1))/))

    b1 = base(4,size(r1))(genData4(size(r1), r1))

    b2 = base(8, size(d1))(genData8(size(d1), d1))

    if ((.not. associated(b1%data)) .or. (.not. associated(b2%data))) &
        error stop 1_4

    if ((size(b1%data%data) /= 1000) .or. (size(b2%data%data) /= 800)) &
        error stop 2_4

    do i = 1, size(r1)
        if (.not. precision_r4(b1%data%data(i), sin(i*1.0))) error stop 3_4
    end do

    do i = 1, size(d1)
        if (.not. precision_r8(b2%data%data(i), dlog(i*1.0d0))) error stop 4_4
    end do
end
