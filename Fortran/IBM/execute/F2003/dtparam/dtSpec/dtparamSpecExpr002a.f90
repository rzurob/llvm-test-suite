!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Variable length type parameter for
!                               derived-type-spec: deferred type parameter for
!                               array case.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (lb, ub)
        integer, len :: lb, ub

        double precision :: data(lb:ub)
    end type

    contains

    subroutine setData (b, d, bounds)
        class(base(:,:)), pointer, intent(out) :: b(:)
        real(8), intent(in) :: d(:)
        integer, intent(in) :: bounds(2)

        integer arraySize

        if (bounds(2) < bounds(1)) then
            nullify(b)
            return
        end if

        arraySize = size(d)/(bounds(2)-bounds(1) + 1)

        allocate(base(bounds(1), bounds(2)) :: b(arraySize))

        do i = 1, arraySize
            b(i)%data = d((i-1)*(bounds(2)-bounds(1)+1)+1:&
                        i*(bounds(2)-bounds(1) + 1))
        end do
    end subroutine
end module

program dtparamSpecExpr002
use m
    class(base(:,:)), pointer :: b1(:)

    double precision :: d1(10000)

    integer lb, ub

    logical(4), external :: precision_r8

    lb = 0
    ub = 200

    call setData (b1, d1, (/2000, 1/))

    if (associated(b1)) error stop 1_4

    d1 = sqrt((/(i*1.0d0, i=1, 10000)/))

    call setData (b1, d1(::2), (/lb, ub/))

    if (.not. associated(b1)) error stop 2_4

    if (size(b1) /= 24) error stop 3_4

    do i = 1, 24
        do j = lb, ub
            associate (k => (i-1)*201 + j -lb + 1)
                if (.not. precision_r8(b1(i)%data(j), sqrt((2*k-1)*1.0d0))) &
                        error stop 4_4
            end associate
        end do
    end do

    !! 2nd call to setData
    ub = ub*3+1

    call setData (b1, d1, (/lb, ub/))

    if (.not. associated(b1)) error stop 5_4

    if (size(b1) /= 16) error stop 6_4

    do i = 1, 16
        do j = lb, ub
            associate (k => (i-1)*602 + j -lb + 1)
                if (.not. precision_r8(b1(i)%data(j), sqrt(k*1.0d0))) &
                        error stop 7_4
            end associate
        end do
    end do
end
