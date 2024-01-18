!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test use of intrinsic MATMUL in intrinsic
!                               assignment for complex(8) allocatables.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface matmul
        function myMatmul (x,m, y, k)
            complex(8), intent(in) :: x(m), y(m,k)
            integer, intent(in) :: m,k

            complex(8) myMatmul(k)
        end function

        function myAnotherMatmul (x, m, y)
            complex(8), intent(in) :: x(:), y(:,:)
            integer, intent(in) :: m

            complex(8) myAnotherMatmul(size(y,2))
        end function
    end interface
end module

program intrinsic004
use m
    complex(8), allocatable :: cx1(:), cx2(:), cx3(:), x(:), y(:,:)

    logical(4), external :: myprecision_x6

    x = cmplx(sqrt([(i*1.d0, i=1, 200)]), log([(i*1.5d0, i=1, 200)]),8)

    allocate (y(0:199, 100))

    do i = 0, 199
        y(i,:) = cmplx(log10([(j*1.5d0, j=i+1,i+100)]), &
            sqrt([(j*1.0d0, j=i+1,i+100)]), 8)
    end do

    cx1 = matmul(x, y)

    cx2 = matmul(x,200, y, 100)

    cx3 = matmul(x,200,y)

    if ((.not. allocated(cx1)) .or. (.not. allocated(cx2)) .or. &
        (.not. allocated(cx3))) error stop 1_4


    if (any([size(cx1), size(cx2), size(cx3)] /= 100)) error stop 2_4

    do i = 1, 100
        if (.not. myprecision_x6 (cx1(i), cx2(i))) error stop 3_4

        if (.not. myprecision_x6 (cx1(i), cx3(i))) error stop 4_4
    end do

    !! 2nd test
    cx1 = matmul(x(::2), y(1::2,::2))
    cx2 = matmul (x(::2), 100, y(1::2,::2))
    cx3 = matmul (x(::2), 100, y(1::2,::2), 50)

    if (any([size(cx1), size(cx2), size(cx3)] /= 50)) error stop 5_4

    do i = 1, 50
        if (.not. myprecision_x6 (cx1(i), cx2(i))) error stop 6_4

        if (.not. myprecision_x6 (cx1(i), cx3(i))) error stop 7_4
    end do
end


function myMatmul (x,m, y, k)
    implicit none
    complex(8), intent(in) :: x(m), y(m,k)
    integer, intent(in) :: m,k

    complex(8) myMatmul(k)

    integer i,j

    myMatmul = 0

    do i = 1, k
        do j = 1, m
            myMatmul(i) = myMatmul(i) + x(j) * y(j,i)
        end do
    end do
end function


function myAnotherMatmul (x, m, y)
    implicit none
    complex(8), intent(in) :: x(:), y(:,:)
    integer, intent(in) :: m

    complex(8) myAnotherMatmul(size(y,2))

    integer i

    if ((size(x) /= m) .or. (size(y,1) /= m)) stop 10

    do i = 1, size(y,2)
        myAnotherMatmul(i) = sum (x(:) * y(:,i))
    end do
end function

logical(4) function myprecision_x6 (cx1, cx2)
    complex(8), intent(in) :: cx1, cx2

    real(8), parameter :: tol = 2d-14

    logical realPartEqual, imagPartEqual

    real(8) dR, dI

    dR = abs(real(cx1,8) - real(cx2,8))
    dI = abs(aimag(cx1)-aimag(cx2))

    realPartEqual = dR *2.0d0 <= abs(real(cx1,8) + real(cx2,8)) *tol
    imagPartEqual = dI *2.0d0 <= abs(aimag(cx1)+aimag(cx2)) *tol

    myprecision_x6 = realPartEqual .and. imagPartEqual
end function
