! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               test the use of pointer arrays as the source
!                               data source for the allocatable component in the
!                               structure constructor.  The pointer is of
!                               bounds-remapped.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base (k)
        integer, kind :: k

        complex(k), allocatable :: cx(:,:)
    end type

    complex(8), target, allocatable :: cx1(:)

    complex(8), pointer :: cc(:,:)

    class(base(8)), allocatable :: b1

    logical(4), external :: precision_x6

    allocate (cx1(100), source=(/(cmplx(0,i,8), i=1,100)/))

    cc (0:2, 1:2) => cx1 (80:1:-1)

    !! test struct constructor in source-expr
    allocate (b1, source=base(8)(cc))

    if (.not. allocated(b1%cx)) error stop 1_4

    if (any(shape(b1%cx) /= (/3,2/))) error stop 2_4

    k = 80

    do j = 1, 2
        do i = 0, 2
            if (.not. precision_x6(b1%cx(i,j), cmplx(0, k,8))) error stop 3_4

            k = k - 1
        end do
    end do

    cc (-1:1, 0:2) => cx1(100:1:-2)

    !! test in associate construct
    associate (x => base(8)(cc))
        if (.not. allocated(x%cx)) error stop 5_4

        if (any (lbound(x%cx) /= (/-1, 0/))) error stop 6_4
        if (any (ubound(x%cx) /= (/1,2/))) error stop 7_4

        k = 100

        do j = 0, 2
            do i = -1, 1
                if (.not. precision_x6(cmplx(0, k, 8), x%cx(i,j))) &
                    error stop 8_4

                k = k - 2
            end do
        end do
    end associate
    end
