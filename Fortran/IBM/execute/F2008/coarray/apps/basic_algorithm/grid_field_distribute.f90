! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       :
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                :
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

program grid_field_distribute
    implicit none
    integer, parameter :: array_size = 20000
    double precision, parameter :: PI = 4*atan(1.0d0)
    integer :: np, i, j, k
    double precision, save :: x(array_size)[*]
    double precision, allocatable :: x1(:)

    np = num_images()

    if (this_image() == 1) then
        allocate (x1(np*array_size))

        do i = 1, np*array_size
            x1(i) = dsin(i*2*pi/(np*array_size))
        end do

        k = 1
        do i = 1, np
            do j = 1, array_size
                x(j)[i] = x1(k)
                k = k + 1
            end do
        end do
    end if

    sync all

    call verify

    sync all !<-- needed since END PROGRAM is not yet implemented

    contains

    logical function my_precision_r8(x, y, rel)
        double precision, intent(in) :: x, y, rel

        my_precision_r8 = abs(x-y) <= abs(x+y)*0.5d0*rel
    end function

    subroutine verify
        integer i

        do i = 1, array_size
            if (.not. my_precision_r8 (x(i), &
                      dsin(((this_image()-1)*array_size+i)*2*pi/(np*array_size)), &
                      1.0d-14)) then
                print *, this_image(),i, (this_image()-1)*array_size+i, x(i)+0.0, dsin(((this_image()-1)*array_size+i)*2*pi/(np*array_size))
                error stop 1_4
            end if
        end do
    end subroutine

end
