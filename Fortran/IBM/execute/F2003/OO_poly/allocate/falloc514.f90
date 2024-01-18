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
!*  DATE                       : 05/16/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate (use of cshift on unlimited poly
!                               entity)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc514
    class(*), allocatable :: x1(:)

    allocate (x1(3), source=(/1_8,2_8,3_8/))

    select type (y => cshift (x1, 1))
        type is (integer(8))
            if (any(y /= cshift ((/1_8,2_8,3_8/), 1))) error stop 1_4
        class default
            error stop 2_4
    end select
end

