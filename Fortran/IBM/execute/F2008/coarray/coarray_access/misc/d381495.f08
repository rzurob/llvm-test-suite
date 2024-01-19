! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-09-27
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 381495
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

program d381495
    implicit none
    real(8), save :: x(10)[*]
    integer :: sz, i
    integer, allocatable :: seeds(:)

    call random_seed (size=sz)

    seeds = [(i + 4096, i = 1, sz)]

    call random_seed (put=seeds)

    call random_number (x)

    call verify_x (x)
    contains

    subroutine verify_x (x1)
        real(8), intent(in) :: x1(10)[*]

        integer i, j, img

        sync all

        do i = 1, 10
            if ( (x1(i) <= 0.0d0) .or. (x1(i) >= 1.0d0)) then
                print *, x1(i), 'out of range between 0 and 1.0'
                error stop 1
            end if

            ! this behavior depends on the compiler to have separate random
            ! number generator per image.  So all the corresponding random
            ! numbers are identical.
            do img = 1, num_images()
                if (x1(i)[img] /= x1(i)) then
                    print *, 'data across images not consistent:', this_image(),img
                    print *, x1(i)[img], 'vs', x1(i)
                    error stop 1
                end if
            end do

            if (i /= 1) then
                do j = i-1, 1, -1
                    if (x1(j) == x1(i)) then
                        print *, 'found repeated values'
                        error stop 1
                    end if
                end do
            end if
        end do
    end subroutine
end
