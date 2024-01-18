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
!*  DATE                       : 05/26/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 286493)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4, allocatable :: data (:)
    end type

    contains

    subroutine testVal1 (b)
        type (base), value :: b

        if (allocated (b%data)) then
            print *, size (b%data)
        else
            print *, 'component not allocated'
        end if
    end subroutine
end module

program fArg009a2
use m
    type (base) b1

    class (base), allocatable :: b2

    allocate (b1%data (5), b2)

    call testVal1 (b1)

    call testVal1 (b2)

    call testVal1 (base((/(k, k=1,100,2)/)))

    call testVal1 (base(null()))
end
