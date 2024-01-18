!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of function/operator in the type
!                               conversion.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    interface operator (+)
        module procedure addB4B8
    end interface

    type(base(4,:)), allocatable :: b1_m

    contains

    function addB4B8 (b4, b8)
        type (base(4,*)), intent(in) :: b4
        type (base(8,b4%n)), intent(in) :: b8

        type (base(4, b4%n)) addB4B8

        addB4B8%data = b4%data + b8%data
    end function

    function sliceB8 (b8, lb, ub)
        type(base(8,*)), intent(in) :: b8
        integer, intent(in) :: lb, ub

        type (base(8,ub-lb+1)) sliceB8

        if ((lb < 1) .or. (ub > b8%n) .or. (ub < lb)) stop 10

        sliceB8%data = b8%data(lb:ub)
    end function
end module

module n
use m
    type container (k, n)
        integer, kind :: k
        integer, len :: n

        type (base(k,n)) :: data
    end type
end module


program dtparamConstr030a
use m
use n
    type (base(8, 450)) :: b1

    type (container(4,:)), allocatable :: co1

    logical(4), external :: precision_r4

    b1%data = (/(1.2*i, i=1,450)/)

    allocate (base(4,220) :: b1_m)

    allocate (container(4,220) :: co1)

    b1_m%data = (/(i*2.2, i = 1, 220)/)

    co1 = container(4,220)(b1_m+sliceB8(b1, 2, 221))

    !! verify co1
    do i = 1, 220
        if (.not. precision_r4 (co1%data%data(i), (1.2+2.2)*i+1.2)) &
                error stop 1_4
    end do
end
