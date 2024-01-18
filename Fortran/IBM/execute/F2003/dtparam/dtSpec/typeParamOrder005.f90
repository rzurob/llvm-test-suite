!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/08/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: For derived type components that are of
!                               procedure pointer; test the parameter order
!                               using init-expr or spec-expr for the parameter
!                               values.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (n, k)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    abstract interface
        function f1(n) result(res)
        import
            integer, intent(in) :: n
            type(A(n,4)) res
        end function

        function f2(n) result(res)
        import
            integer, intent(in) :: n
            type(A(n,8)) res
        end function
    end interface

    type :: base (k, n, ka, na)
        integer, kind :: k, ka
        integer, len :: n, na

        complex(k) :: cx(n)
!        procedure(type(A(na, ka))), pointer, nopass :: p1 => null()
        procedure(f1), pointer, nopass :: p1 => null()
        procedure(f2), pointer, nopass :: p2 => null()
    end type

    contains

    !! the declaration of the dummy-arg b is unusual
    subroutine getBaseVal4_4 (cx, r1, b)
        complex(4), intent(inout) :: cx(:)
        real(4), intent(inout) :: r1(:)
        type (base(4,size(cx),4, size(r1))), intent(in) :: b

        type(A(size(r1),4)) :: a1_temp

        cx = b%cx

        if (associated(b%p1)) then
            a1_temp = b%p1(25)

            r1 = a1_temp%data
        end if
    end subroutine

    subroutine normalizeBaseVal8_8 (b, cx, d1)
        type (base(8,*,8,*)), intent(in) :: b
        complex(8), intent(inout) :: cx(b%n)
        double precision, intent(inout) :: d1(b%na)

        type (A(b%na, 8)) a1_temp

        real(8) denominator

        denominator = abs(sum(b%cx))

        cx = b%cx /denominator

        if (associated(b%p2)) then
            a1_temp = b%p2(30)

            denominator = abs(sum(a1_temp%data))

            d1 = a1_temp%data/denominator
        end if
    end subroutine
end module

program typeParamOrder005
use m
    type (base(4,10, 4,25)) :: b1
    type (base(8,28, 8,30)), allocatable :: b2

    real(4) :: r1(25)
    real(8) :: d1(30)
    complex(4), allocatable :: cx1(:)
    complex(8), pointer :: cx2(:)

    procedure(f1) :: genA25_4
    procedure(f2) :: genA30_8

    logical(4), external :: precision_r4, precision_r8, precision_x8, &
                            precision_x6

    allocate (b2)
    allocate (cx1(10), cx2(28))

    b1%cx = (/((i*1.0e0, i*1.0e1), i=1,10)/)
    b2%cx = (/(i*1.0d1, i=1, 28)/)

    b1%p1 => genA25_4
    b2%p2 => genA30_8

    call getBaseVal4_4 (cx1, r1, b1)
    call normalizeBaseVal8_8 (b2, cx2, d1)

    !! verify results
    do i = 1, 10
        if (.not. precision_x8(cx1(i), (i*1.0e0, i*1.0e1))) error stop 1_4
    end do

    do i = 1, 25
        if (.not. precision_r4 (r1(i), i*1.0)) error stop 2_4
    end do

    do i = 1, 28
        if (.not. precision_x6(cx2(i), (i*1.0d1,0.0d0)/4.06d3)) &
                    error stop 3_4
    end do

    do i = 1, 30
        if (.not. precision_r8(d1(i), (31-i)*1.0d0/4.65d2)) error stop 4_4
    end do
end

function genA25_4 (n)
use m
    integer, intent(in) :: n
    type (A(n, 4)) genA25_4

    if (n /= 25) stop 10
    genA25_4%data = (/(i*1.0, i=1,25)/)
end function

function genA30_8 (n)
use m
    integer, intent(in) :: n
    type (A(n,8)) genA30_8

    if (n /= 30) stop 12
    genA30_8%data = (/(i*1.0d0, i=30,1,-1)/)
end function
