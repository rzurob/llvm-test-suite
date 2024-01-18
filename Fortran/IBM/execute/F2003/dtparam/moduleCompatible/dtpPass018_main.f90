!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/17/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               type bound procedure (A test case uses the
!                               abstract data type and a object container
!                               containing this type; test sorting algorithm)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtpPass018
use dataMod
use arrayTempMod
    implicit none
    type(arrayTemp) at1

    integer i, n, arraySize

    n = 1024

    arraySize = 1024

    allocate (dataArray(n=n):: at1%data(arraySize))

    do i = 1, arraySize
        at1%data(i) = dataArray(n=n)(arraySize-i)
    end do

    call at1%sort

    !! now verify the sorted results of at1
    select type (x => at1%data)
        type is (dataArray(n=*))
            do i = 1, arraySize
                if (any(x(i)%values /= i-1)) error stop 1_4

                if (sum(x(i)%values) /= (i-1) * n) error stop 2_4
            end do

        class default
            error stop 3_4
    end select
end
