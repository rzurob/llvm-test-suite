
module m
    type base
        complex(8), allocatable :: cx(:)
    end type

    type, extends(base) :: child
        character(:), pointer :: names(:)
    end type
end module


program dummyArg008
use m
    interface assgn
        subroutine assgnBase (b1, b2)
        import base
            type(base), allocatable :: b1(:)
            type (base) b2(:)
        end subroutine

        subroutine assgnChild (c1, c2)
        import child
            type(child), allocatable :: c1(:)
            type(child) c2(:)
        end subroutine
    end interface

    type(base), allocatable :: b1(:)
    type(child), allocatable :: c1(:), c2(:)

    logical(4), external :: precision_x6

    allocate (c2(10))

    do i = 1, 10
        c2(i)%cx = [(cmplx(log(j*1.0d0), log(j*1.0d1),8), j=1, i*10)]

        allocate (c2(i)%names(i), source='test ' // [(achar(47+j), j=1,i)])
    end do

    call assgn (c1, c2)

    call assgn (b1, c2(::2)%base)

    if ((size(c1) /= 10) .or. (size(b1) /= 5) .or. (size(c2) /= 10)) &
            error stop 1_4

    do i = 1, 5
        if (size(b1(i)%cx) /= 20*i-10) error stop 2_4

        do j = 1, 20*i-10
            if (.not. precision_x6(b1(i)%cx(j), &
                cmplx(log(j*1.0d0), log(j*1.0d1),8))) error stop 3_4
        end do
    end do

    do i = 1, 10
        if (.not. associated(c1(i)%names, c2(i)%names)) error stop 4_4

        if ((size(c1(i)%cx) /= i*10) .or. (size(c2(i)%cx) /= i*10)) &
                error stop 5_4

        do j = 1, 10*i
            if (.not. precision_x6(c1(i)%cx(j), &
                cmplx(log(j*1.0d0), log(j*1.0d1),8))) error stop 6_4


            if (.not. precision_x6(c1(i)%cx(j), c2(i)%cx(j))) error stop 7_4
        end do

        if (size(c1(i)%names) /= i) error stop 8_4

        do j = 1, i
            if (c2(i)%names(j) /= 'test '//achar(47+j)) error stop 9_4
            if (c1(i)%names(j) /= 'test '//achar(47+j)) error stop 10_4
        end do
    end do
end


subroutine assgnBase (b1, b2)
use m, only: base
    type(base), allocatable :: b1(:)
    type (base) b2(:)

    b1 = b2
end subroutine

subroutine assgnChild (c1, c2)
use m, only: child
    type(child), allocatable :: c1(:)
    type(child) c2(:)

    c1 = c2
end subroutine
