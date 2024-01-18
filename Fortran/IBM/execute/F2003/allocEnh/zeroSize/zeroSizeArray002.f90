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
!*  DATE                       : 08/28/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Zero size array allocated as the result of the
!                               intrinsic assignment; test character type and
!                               complex type.  Test the deferred char type as
!                               well.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program zeroSizeArray002
    character(20), allocatable :: c1(:)
    character(:), allocatable :: c2(:), c3(:,:,:,:,:)

    complex(4), allocatable :: cx1(:)
    complex(8), allocatable :: cx2(:,:)


    !! test1 : character type
    c1 = (/character(10) :: /)

    c2 = (/character(30) :: /)

    c3 = reshape((/character(20) :: /), (/0,2,3,4,5/))

    if ((.not. allocated(c1)) .or. (.not. allocated(c2)) .or. &
        (.not. allocated(c3))) error stop 1_4

    if (size(c1) /= 0) error stop 2_4

    if ((size(c2) /= 0) .or. (len(c2) /= 30)) error stop 3_4

    if ((size(c3) /= 0) .or. (len(c3) /= 20)) error stop 4_4

    if (any(shape(c3) /= (/0,2,3,4,5/))) error stop 5_4


    !! test2 : complex type
    cx1 = (/complex :: /)

    cx2 = reshape (cx1,(/size(cx1)/2, 2/))

    if ((.not. allocated(cx1)) .or. (.not. allocated(cx2))) error stop 6_4

    if (any(shape(cx2) /= (/0,2/))) error stop 7_4
end
