! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/funcResult/intrinsic003.f
! opt variations: -qck -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/04/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test use of the intrinsic result from intrinsic
!                               function UNPACK of the derived type in an
!                               intrinsic assignment for allocatable variable.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,k2)    ! (2,8)
        integer, kind :: k1,k2
        integer(k1)      divNo
        integer(k2)      empNo
    end type

    type, extends(base) :: child    ! (2,8)
        character(:), allocatable :: name
    end type
end module

program intrinsic003
use m
    type(child(2,8)), allocatable :: c1(:,:,:), c2(:)

    logical, allocatable :: mask(:,:,:)

    c2 = [(child(2,8)(i, i+1000, repeat('I', i)), i=1, 30)]

    mask = unpack ([(mod(i,2)==0, i=1,60)], &
        reshape([(.true., i=1,60)], [3,4,5]), .false.)


    c1 = unpack (c2, mask, FIELD = child(2,8)(0,0, ''))

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
