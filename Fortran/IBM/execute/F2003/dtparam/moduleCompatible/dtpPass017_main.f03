
program dtpPass017
use m1
    implicit none
    type(coArraySim(:,:)), allocatable :: cas
    type(coArraySim(:,:)), allocatable :: cas1, cas2

    integer coSize, imgNo, i, j, k
    logical(4), external :: precision_r8

    real(8), allocatable :: d1(:), d2(:)

!    coSize = 100
!    imgNo = 10
    read (*,*) coSize, imgNo

    !! say we make an array of 100 elements on 10 "images"
    allocate (coArraySim(coSize, imgNo) :: cas)

    !! set up values for cas
    do i = 1, imgNo
        do j = 1, coSize
            cas%data(i)%data(j) = sqrt(1.0d0*j + 1.0d3*i)
        end do
    end do

    cas1 = cas%cshift(1)
    cas2 = cas%cshift(-2)

    if ((.not. allocated(cas1)) .or. (.not. allocated(cas2))) error stop 1_4

    if ((cas1%n /= coSize) .or. (cas1%m /= imgNo)) error stop 2_4
    if ((cas2%n /= coSize) .or. (cas2%m /= imgNo)) error stop 3_4

    !! verify results of cas1
    do i = 1, imgNo
        do j = 1, coSize-1
            if (.not. precision_r8(cas1%data(i)%data(j), &
                    sqrt(1.0d0*(j+1) + 1.0d3*i))) error stop 4_4
        end do

        !! the last element in the local array is shifted from array right to it
        if (i /= imgNo) then
            if (.not. precision_r8(cas1%data(i)%data(coSize), &
                    sqrt(1.0d3*(i+1) + 1))) error stop 5_4
        end if
    end do

    if (.not. precision_r8(cas1%data(imgNo)%data(coSize), &
            sqrt(1001.0d0))) error stop 6_4

    !! verify results of cas2
    do i = 1, imgNo
        do j = 3, coSize
            if (.not. precision_r8(cas2%data(i)%data(j), &
                    sqrt(1.0d0*(j-2)+1.0d3*i))) error stop 7_4
        end do

        !! the 1st two elements are shifted from left image's last two elements
        if (i /= 1) then
            if (.not. precision_r8(cas2%data(i)%data(2), &
                    sqrt(1.0d0*coSize+1.0d3*(i-1)))) error stop 8_4

            if (.not. precision_r8(cas2%data(i)%data(1), &
                    sqrt(1.0d0*(coSize-1)+1.0d3*(i-1)))) error stop 9_4
        end if
    end do

    if (.not. precision_r8(cas2%data(1)%data(1), &
            sqrt(1.0d0*(coSize-1) + 1.0d3*imgNo))) error stop 10_4

    if (.not. precision_r8(cas2%data(1)%data(2), &
            sqrt(1.0d0*coSize + 1.0d3*imgNo))) error stop 11_4

    !! verify cas1, cas2 in a different way
    d1 = [(cas%data(i)%data, i = 1, imgNo)]

    d2 = cshift(d1, -2)
    d1 = cshift(d1, 1)

    k = 1

    do i = 1, imgNo
        do j = 1, coSize
            if (.not. precision_r8(cas1%data(i)%data(j), d1(k))) error stop 15_4
            if (.not. precision_r8(cas2%data(i)%data(j), d2(k))) error stop 16_4

            k = k + 1
        end do
    end do
end
