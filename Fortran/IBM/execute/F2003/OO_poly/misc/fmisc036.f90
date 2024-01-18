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
!*  DATE                       : 04/13/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous item (defect 289822)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        character (20) :: name
    end type

    contains

    subroutine printBase (b)
        type (base), intent(in) :: b

        print *, b%name
    end subroutine
end module

program fmisc036
use m
    type (base), allocatable :: b1(:)
    type (base)  :: b2(1:2)

    allocate (b1(2))
    b1%name = 'test'

    b2%name = 'xlf'

    associate (x2 => (/b1/))
        call printBase (x2(1))
        call printBase (x2(2))
    end associate

    associate (x3 => (/b2/))
        call printBase (x3(2))
    end associate
end

