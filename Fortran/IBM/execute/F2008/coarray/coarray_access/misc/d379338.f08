! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-09
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 379338
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
    implicit none
    integer, parameter :: array_size = 1024
    real(4), save :: cox(array_size)[*]
    real(4), allocatable:: x(:)
    integer np, i, j, k, me
    logical, external :: precision_r4

    np = num_images()
    me = this_image()

    if (this_image() == 1) then
        allocate (x(num_images() * array_size))
        do i = 1, array_size*num_images()
            x(i) = i
        end do

        k = 1
        do i = 1, np
            do j = 1, array_size
                cox(j)[i] = x(k)
                k = k + 1
            end do
        end do
    end if

    sync all

    do i = 1, array_size
        if (.not. precision_r4(cox(i), i*1.0+(me-1)*array_size)) then
            print *, 'verification fails on image',me, 'for element', i
            print *, cox(i),'vs', i*1.0+(me-1)*array_size
            error stop 1
        end if
    end do

end

