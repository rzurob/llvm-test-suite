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
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : ALLOCATE statement (allocation of zero-length
!                               characters)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc513a4
    class(*), allocatable :: x1
    class (*), pointer :: x2(:)

    allocate (character(0) :: x1)
    allocate (character(-10) :: x2(10))

    if (.not. allocated(x1)) error stop 4_4
    if (.not. associated(x2)) error stop 5_4

    select type (x1)
        type is (character(*))
            if (len(x1) /= 0) error stop 1_4
            if (x1 /= '') error stop 2_4

        class default
            error stop 3_4
    end select


    select type (x2)
        type is (character(*))
            if (len(x2) /= 0) error stop 6_4
            if (size(x2) /= 10) error stop 7_4

            if (any (x2 /= '')) error stop 8_4
        class default
            error stop 10_4
    end select
end
