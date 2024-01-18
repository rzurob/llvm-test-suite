!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/21/2006
!*
!*  DESCRIPTION                : miscellaneous (ICE in xlfcode @ -O4)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point4! (k)
        real(4) :: x, y
    end type

    type point8! (k)
        real(8) :: x, y
    end type

    interface operator(-)
        module procedure negate4
        module procedure negate8
    end interface

    contains

    elemental type(point4) function negate4 (p1)
        type(point4), intent(in) :: p1

        negate4%x = -p1%x
        negate4%y = -p1%y
    end function

    type(point8) elemental function negate8 (p1)
        type(point8), intent(in) :: p1

        negate8 = point8(-p1%x, -p1%y)
    end function
end module

program dtparamOperator001
use m
    type(point8) :: b1(60,50)
    class(point8), allocatable :: b2(:,:)

    type (point4), pointer :: b3(:)

    logical(4), external :: precision_r8, precision_r4

    forall (i=1:60, j=1:50)
        b1(i,j) = -point8(-i*10-j, y=log(1.0d0*(i+j*10)))
    end forall

    allocate (b2(0:59,0:49), source=-b1)

    allocate (b3(200), source=-(/(point4(x=i, y=1.0/i), i=1, 200)/))


    !! verify
    do j = 0,49
        do i = 0,59
            if (.not. precision_r8(b2(i,j)%x, -1.0d0*(10*i+j+11))) &
                    error stop 1_4

            if (.not. precision_r8(b2(i,j)%y, log(1.0d0*(i+10*j+11)))) &
                    error stop 2_4
        end do
    end do

    do i = 1, 200
        if (.not. precision_r4(b3(i)%x, -i*1.0)) error stop 3_4

        if (.not. precision_r4(b3(i)%y, -1.0/i)) error stop 4_4
    end do
end
