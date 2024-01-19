! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               structure constructor (array component in
!                               struture constructor)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type intArray (n)
        integer, len :: n

        integer :: data(n)
    end type
end module

module n
    type realType (k)
        integer, kind :: k

        real(k) :: data
    end type
end module


module p
use m
use n
    type dataType (k)
        integer, kind :: k

        class(intArray(:)), allocatable :: intData(:)
        class(realType(k)), allocatable :: realData(:,:)
    end type
end module

program dtparamConstr057
use p
    type(dataType(4)), allocatable :: d1
    type (dataType(16)) d2 (10)

    integer intArraySize
    integer m, n

    logical(4), external :: precision_r4, precision_r6

    intArraySize = 126

    m = 15
    n = 18

    d1 = dataType(4)([(intArray(intArraySize)([(j+1000*i, j=1,intArraySize)]), &
        i=1, 8)], reshape([(realType(4)(i), i=1,m*n)], [m,n]))

    do i = 1, 10
        allocate (d2(i)%intData(i), source=[(intArray(intArraySize/i)&
            (data=[(i*10000+j*1000+k, k=1, intArraySize/i)]), j = 1, i)])

        allocate (d2(i)%realData(i,i), source=realType(16)(i*i))
    end do

    !! verify d1
    if (d1%intData%n /= intArraySize) error stop 10_4

    do i = 1, 8
        do j = 1, intArraySize
            if (d1%intData(i)%data(j)-1000*i /= j) error stop 1_4
        end do
    end do

    k = 1
    do j = 1, n
        do i = 1, m
            if (.not. precision_r4(d1%realData(i,j)%data, k*1.0_4)) error stop 2_4
            k = k + 1
        end do
    end do

    !! verify d2
    do i = 1, 10
        if (d2(i)%intData%n /= intArraySize/i) error stop 11_4

        do j = 1, i
            do k = 1, intArraySize/i
                if (d2(i)%intData(j)%data(k) /= i*10000+j*1000+k) error stop 3_4
            end do

            do k = 1, i
                if (.not. precision_r6(d2(i)%realData(k,j)%data, i*i*1.0_16)) &
                        error stop 4_4
            end do
        end do
    end do
end
