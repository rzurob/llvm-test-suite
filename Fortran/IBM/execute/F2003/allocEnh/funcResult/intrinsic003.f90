!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 10/04/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test use of the intrinsic result from intrinsic
!                               function UNPACK of the derived type in an
!                               intrinsic assignment for allocatable variable.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(2) divNo
        integer(8) empNo
    end type

    type, extends(base) :: child
        character(:), allocatable :: name
    end type
end module

program intrinsic003
use m
    type(child), allocatable :: c1(:,:,:), c2(:)

    logical, allocatable :: mask(:,:,:)

    c2 = [(child(i, i+1000, repeat('I', i)), i=1, 30)]

    mask = unpack ([(mod(i,2)==0, i=1,60)], &
        reshape([(.true., i=1,60)], [3,4,5]), .false.)


    c1 = unpack (c2, mask, FIELD = child(0,0, ''))

    if (any(lbound(c1) /= 1) .or. any(ubound(c1) /= [3,4,5])) error stop 1_4

    ival = 1

    do k = 1, 5
        do j = 1, 4
            do i = 1, 3
                if (.not. allocated(c1(i,j,k)%name)) error stop 15_4

                if (mod(ival,2) == 1) then
                    if (c1(i,j,k)%divNo /= 0) error stop 2_4

                    if (c1(i,j,k)%empNo /= 0) error stop 3_4

                    if (c1(i,j,k)%name%len /= 0) error stop 4_4
                else
                    if (c1(i,j,k)%divNo /= (ival+1)/2) error stop 5_4

                    if (c1(i,j,k)%empNo /= 1000 + (ival+1)/2) error stop 6_4

                    if (c1(i,j,k)%name /= repeat('I', (ival+1)/2)) &
                        error stop 7_4
                end if

                ival = ival + 1
            end do
        end do
    end do
end
