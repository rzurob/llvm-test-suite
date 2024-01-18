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
!*  DATE                       : 02/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Simple test on type-spec in array
!                               constructor; and generic resolution based on
!                               kind type parameters.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k = 8
        integer, len :: n = 38

        real(k) :: data(n)
    end type

    interface printBase
        module procedure printBaseArray4
        module procedure printBaseArray8
    end interface

    contains

    subroutine printBaseArray4 (b)
        type (base(4, *)), intent(in) :: b(:)

        print *, 'kind = 4'

        do i = 1, size(b)
            write (*, '(6g15.5)') b(i)%data
        end do
    end subroutine

    subroutine printBaseArray8 (b)
        type (base(8, *)), intent(in) :: b(:)

        print *, 'kind = 8'

        do i = 1, size(b)
            write (*, '(6g15.7)') b(i)%data
        end do
    end subroutine
end module

program dtparamDefVal016
use m
    call printBase ((/base(8,38) ::/))
    call printBase ((/base(4, 100) :: /))
end
