! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-11-09
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : a simple test to verify that the scalar
!                               coindexed object as an actual argument is copied
!                               out to the remote image.
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
    real(8), save :: x1(10)[*]
    real(8), save :: x2(10)[*]
end module

module op_mod
    implicit none
    contains

    subroutine copy_N_double_value (a, b, img, ind)
        real(8), intent(inout) :: a(*)[*], b
        integer, intent(in) :: img, ind

        b = a(ind)[img]

        a(ind)[img] = a(ind)[img] + b
    end subroutine
end module

program test_copy_out_scalar03
use data_mod
use op_mod
    implicit none

    integer me, np, left, i
    np = num_images()
    me = this_image()

    if (me == 1) then
        left = np
    else
        left = me - 1
    end if

    x1(:) = me*[(i,i=1,size(x1))]
    sync all

    do i = 1, size(x1)
        call copy_N_double_value (x1, x2(i)[left], left, i)
    end do
    sync all
    call verifyAll

    contains

    subroutine verifyAll
        logical, external :: precision_r8

        do i = 1, size(x1)
            if (.not. precision_r8(x2(i), me*1.0_8*i)) then
                print *, 'Failed verify x2 on image',me,'for element',i
                print *, x2(i),'vs', me*1.0_8*i
                error stop 1
            end if

            if (.not. precision_r8(x1(i), x2(i)*2.0_8)) then
                print *, 'Failed verify x1 on image',me,'for element',i
                print *, x2(i),'vs', me*2.0_8*i
                error stop 1
            end if
        end do
    end subroutine
end

