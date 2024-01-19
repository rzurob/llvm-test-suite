! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use the unlimited poly pointer component
!                               to be associated with an object of parameterized
!                               derived type with target attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n
    type container
        class(*), pointer :: data(:,:)
    end type
end module

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type
end module

program dtparamConstr039
use m
use n
    type(container) :: co1(10)

    class(base(4,:)), allocatable, target :: b1(:,:)

    real(4) :: r1(300)

    type (base(8, 35)), target :: b2(0:3,0:15)

    logical(4), external :: precision_r4, precision_r8

    allocate (base(4,25) :: b1(3, -1:2))

    co1(1) = container(data = b1)

    co1(3) = container(b2)

    r1 = sqrt(1.0*(/(i, i=1,300)/))

    if (any(lbound(co1(1)%data) /= (/1, -1/)) .or. &
        any(ubound(co1(1)%data) /= (/3,2/))) error stop 1_4


    select type (x => co1(1)%data)
        type is (base(4,*))
            select type (y => co1(3)%data)
                class is (base(8,*))
                    do j = 0, 15
                        do i = 0, 3
                            do k = 1, 35
                                y(i,j)%data(k) = log(1.0d0*(i*100+3*j+k))
                            end do
                        end do
                    end do

                    !! use co1(3)'s data in calculating co1(1)
                    icount = 1

                    do i = 1, 3
                        do k = 1, 25
                            do j = -1, 2
                                x(i,j)%data(k) = r1(icount) + y(i-1,j+1)%data(k)

                                icount = icount + 1
                            end do
                        end do
                    end do

                class default
                    error stop 3_4
            end select

        class default
            error stop 2_4
    end select


    !! verify b1 and b2
    do k = 1, 35
        do i = 0, 3
            do j = 0, 15
                if (.not. precision_r8 (b2(i,j)%data(k), &
                    log(1.0d0*(i*100+3*j+k)))) error stop 5_4
            end do
        end do
    end do

    icount = 1

    do i = 1, 3
        do k = 1, 25
            do j = -1, 2
                if (.not. precision_r4(b1(i,j)%data(k), &
                    real(r1(icount)+b2(i-1,j+1)%data(k), kind=4))) error stop 6_4

                icount = icount + 1
            end do
        end do
    end do

end
