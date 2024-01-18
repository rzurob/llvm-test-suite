! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-01-10
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : The test case tests the 3-d complex(8)
!                               coindexed object access (assignment).
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
module data_mod
    implicit none
    complex(8), save :: dcx1(3,3,4)[0:*]
end module

module op_mod
    implicit none
    contains

    subroutine set_cmplx8(x1, i,j,k)
        integer, intent(in) :: i,j,k
        complex(8), intent(out) :: x1(i,j,k)[0:*]

        complex(8), save :: dcx2[*] = cmplx(1.0d0, 0.0d0, 8)

        integer :: me, np, left, right
        integer l1,l2,l3

        me = this_image(x1, 1)
        np = num_images()

        if (me == 0) then
            left = np - 1
        else
            left = me - 1
        end if

        do l3 = 1, k
            do l2 = 1, j
                do l1 = 1, i
                    x1(l1,l2,l3)[left] = cmplx(0.0d0, 1.0d0*(left+1)*(l1*100+l2*10+l3), 8) + dcx2*(left+1)
                end do
            end do
        end do
        sync all
    end subroutine
end module

program assign_cmplx3
    use data_mod, only : dcx1
    use op_mod, only : set_cmplx8
    implicit none

    integer :: me, np, i, j, k, right
    logical, external :: precision_x6

    np = num_images()
    me = this_image(dcx1, 1)

    if (np < 2) then
        print *, 'Error: requires at least two images to run'
        error stop 1
    end if

    if (me == np - 1) then
        right = 0
    else
        right = me + 1
    end if

    call set_cmplx8 (dcx1, size(dcx1,1),size(dcx1,2),size(dcx1,3))

    do k = 1, size(dcx1,3)
        do j = 1, size(dcx1,2)
            do i = 1, size(dcx1,1)
                if (.not. precision_x6(dcx1(i,j,k)[right], &
                    cmplx(1.0d0, 1.0d0*(100*i+10*j+k),8)*(right+1))) then

                    print *, 'Error: failed to verify dcx1 for image', right+1
                    print *, 'element:(',i,j,k,').  Expected:', cmplx(1.0d0, &
                    1.0d0*(100*i+10*j+k),8)*(right+1), '. Actual:',dcx1(i,j,k)

                    error stop 1
                end if
            end do
        end do
    end do
end
