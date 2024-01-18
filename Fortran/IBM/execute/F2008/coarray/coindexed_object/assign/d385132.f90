! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-17
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : defect 385132
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

program d385132
    implicit none

    real, save :: d1(10,10)[*]
    real, save :: d2(10,10)[*]
    integer me, i, np

    me = this_image()
    np = num_images()

    if (np < 1) then
        print *, 'Error: program requires at least 2 images to run'
        error stop 1
    end if

    d1 = -1.0
    d2 = reshape ([(i, i = 1, 10*10)]*this_image(), [10,10])

    sync all

    if (me == 1) then
        call foo (d1, d2(::2,::2)[np], np)
    else
        call foo (d1, d2(::2,::2)[me-1], me-1)
    end if

    call verify_d1

    contains

    subroutine foo (d, dd, img)
        real, intent(inout) :: d(10,10)[*]
        real, intent(in) :: dd (:,:)
        integer, intent(in) :: img

        d(::2,::2)[img] = dd
        sync all
    end subroutine

    subroutine verify_d1
        integer i, j, k
        logical, external :: precision_r4

        k = 1
        do j = 1, 10
            do i = 1, 10
                if ((mod(i,2) == 1) .and. (mod(j,2) == 1)) then
                    if (.not. precision_r4 (d1(i,j), k*1.0*me)) then
                        print *, 'Error: failed to verify d1 on image', me
                        print *, 'element (',i,',',j,'). Expected:', k*me*1.0, &
                             'Actual:', d1(i,j)
                        error stop 1
                    end if
                else
                    if (.not. precision_r4 (d1(i,j), -1.0)) then
                        print *, 'Error: failed to verify d1 on image', me
                        print *, 'element (',i,',',j,'). Expected:', -1.0, &
                             'Actual:', d1(i,j)
                        error stop 1
                    end if
                end if

                k = k + 1
            end do
        end do
        print *, 'done'
    end subroutine
end
