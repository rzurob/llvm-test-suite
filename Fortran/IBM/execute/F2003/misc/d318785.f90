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
!*  DATE                       : 04/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 318785)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type(base), intent(inout) :: b

        print *, 'reset id from', b%id, 'to -1'
        b%id = -1
    end subroutine
end module

module m1
use m
    type container
        type(base), allocatable :: data
    end type

    contains

    subroutine printCo (co)
        type(container), value :: co

        if (allocated(co%data)) then
            print *, co%data%id

            deallocate(co%data)
        end if
        print *, 'done'
    end subroutine
end module

use m1
    call printCo (container(base(100)))
end
