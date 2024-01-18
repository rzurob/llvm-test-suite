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
!*  DATE                       : 01/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (C544: assumed-size array
!                               with INTENT(OUT), allow derived type without
!                               default initialization)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg604
    type base
        integer :: i
    end type

    type (base) :: b1 (10)

    call abc (b1(3))

    if (any (b1(3:5)%i /= (/1,2,3/))) error stop 1_4

    contains

    subroutine abc (b)
        class(base), intent(out) :: b(*)

        b(1:3)%i = (/1,2,3/)
    end subroutine
    end
