!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: A correct use of structure constructor for
!                               type conversion: use functions instead of
!                               defined assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    interface convert
        procedure assgnBase4toBase8
    end interface

    contains

    function assgnBase4toBase8 (b4) result (b8)
        type(base(4,*)), intent(in) :: b4

        type(base(8,b4%n)) :: b8

        b8%data = b4%data
    end function
end module

module n
use m
    type container (k, n)
        integer, kind :: k
        integer, len :: n

        type(base(k,n)) :: data
    end type
end module

program dtparamConstr030
use m
use n
    type (base(8,:)), allocatable :: b1
    type (base(4,:)), allocatable :: b2

    type(container(8,:)), allocatable :: co1

    logical(4), external :: precision_r4

    allocate (base(8,200) :: b1)
    allocate (base(4,200) :: b2)
    allocate (container(8,200) :: co1)

    b2%data = exp(log10((/(i*1.0, i=1,200)/)))

    co1 = container(8,200)(convert(b2))

    !! verify
    do i = 1, 200
        if (.not. precision_r4(real(co1%data%data(i), 4), exp(log10(i*1.0)))) &
                error stop 1_4
    end do
end
