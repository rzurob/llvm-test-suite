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
!*  DATE                       : 04/21/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (named constants used as
!                               the data source for the allocatable components)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data
    end type
end module

program fconstr033a6
use m
    character(20), parameter :: comment = '!1234567890987654321'

    type (base) b1

    b1 = base (comment(3:5))

    if (.not. allocated (b1%data)) error stop 1_4

    select type (x => b1%data)
        type is (character(*))
            if (len(x) /= 3) error stop 2_4

            print *, x
        class default
            error stop 3_4
    end select
end
