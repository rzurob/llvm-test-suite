! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (A data type that does array
!                               transformation for its data: matmul.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module math
    type dataType (n)
        integer, len :: n

        integer(8) :: data(n)

        contains

        procedure :: equal => dataEqualD1
    end type

    type specialMath (n, n1, n2)
        integer, len :: n, n1, n2

        type(dataType(n)) matrix(n1, n2)

        contains

        procedure :: matmul => matmul4DataType
        procedure :: at => getDatawithIdex
    end type

    contains

    function dataEqualD1 (dt1, d1)
        class(dataType(*)), intent(in) :: dt1
        integer(8), intent(in) :: d1(dt1%n)

        logical dataEqualD1 (dt1%n)

        dataEqualD1 = d1 == dt1%data
    end function

    function getDatawithIdex (dt1, index1, index2) result (res)
        class(specialMath(*,*,*)), intent(in) :: dt1
        integer, intent(in) :: index1, index2

        type(dataType(dt1%n)) res

        res = dt1%matrix(index1, index2)
    end function

    function matmul4DataType (dt1, dt2)
        class(specialMath(*,*,*)), intent(in) :: dt1, dt2

        type (specialMath(dt1%n, dt1%n1, dt2%n2)) matmul4DataType

        if (dt1%n /= dt2%n) error stop 1

        if (dt1%n2 /= dt2%n1) error stop 2

        !
        do i = 1, dt1%n
            do j = 1, dt1%n1
                do k = 1, dt2%n2
                    matmul4DataType%matrix(j,k)%data(i) = 0

                    do l = 1, dt1%n2
                        matmul4DataType%matrix(j,k)%data(i) = &
                            matmul4DataType%matrix(j,k)%data(i) +  &
                            dt1%matrix(j,l)%data(i)*dt2%matrix(l,k)%data(i)
                    end do
                end do
            end do
        end do
    end function
end module


program dtpPass015
use math
    integer, parameter :: dim1 = 512, dim2 = 1024, blockSize = 10

    type (specialMath(blockSize, dim1, dim2)) sm1
    type (specialMath(blockSize, dim2, dim1)) sm2

    type (specialMath(:,:,:)), allocatable :: sm3

    integer(8)  verifyArray (blockSize, dim1, dim1)

    !! set up data for sm1 and sm2
    do i = 1, blockSize
        do j = 1, dim1
            sm1%matrix(j,:)%data(i) = [(i+j/10+k/100, k = 1, dim2)]

            sm2%matrix(:,j)%data(i) = sm1%matrix(j,:)%data(i)* &
                    [((-1)**k, k = 1, dim2)]
        end do
    end do

    sm3 = sm1%matmul(sm2)

    if ((sm3%n /= blockSize) .or. (sm3%n1 /= dim1) .or. (sm3%n2 /= dim1)) &
            error stop 1_4


    !! verify the results
    do i = 1, blockSize
        verifyArray(i,:,:) = matmul(sm1%matrix(:,:)%data(i), sm2%matrix(:,:)%data(i))
    end do

    do i = 1, dim1
        do j = 1, dim1
            associate (x => sm3%at (i,j))
                if (.not. all(x%equal (verifyArray(:,i,j)))) error stop 2_4
            end associate
        end do
    end do
end
