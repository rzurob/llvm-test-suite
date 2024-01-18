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
!*  DATE                       : 04/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 319004)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    contains

    subroutine ttt (c, c1)
        character(10), value, target :: c
        character(*), intent(in) :: c1

        character(:), pointer :: cc

        cc => c

        if (len(cc) /= len(c1)) stop 5

        if (cc /= c1) stop 10
    end subroutine
end module

use m
    character(:), allocatable, target :: c

    allocate (character(10) :: c)

    c(:) = 'xlftest 101'

    call ttt (c, c)

    call ttt (c, 'xlftest 10')

    call ttt ('xlftest 10', c)

    call ttt ('xlftest 10', 'xlftest 10')
end
