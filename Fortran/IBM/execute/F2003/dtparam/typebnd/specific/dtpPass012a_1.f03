! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/21/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               A test case tests the sorting algorithm of
!                               dataType used in dtpPass012a
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtpPass012a_1
use m1
    type (flagedData(8)), pointer :: fd1(:)
    real(8), parameter :: pi = 4.0_8 * atan (1.0_8)

    allocate (fd1(50))

    do i = 1, 50
        fd1(i)%data = sin (i*pi/25.0_8)

        fd1(i)%flag = mod (i, 2) == 0
    end do

    !! get the array sorted descendingly
    call sortFlagedDataArrayDown (fd1(:12))

    do i = 1, 12
        if (fd1(i)%isSet()) then
            call fd1(i)%print

            if (fd1(i) .lt. fd1(i+1)) error stop 10
        else
            call fd1(i)%dataType%print

            if (.not. (fd1(i) < dataType(8)(1.0_8))) error stop 20
        end if
    end do
end
