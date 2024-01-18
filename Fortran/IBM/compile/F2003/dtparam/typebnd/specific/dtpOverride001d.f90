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
!*  DATE                       : 06/04/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound procedures: Test the
!                               dummy-arg's characteristic checking: kind type
!                               parameter for passed-object dummy-arg must be
!                               the same.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        contains

        procedure :: p => procBase
    end type

    type, extends(base) :: child
        contains

        procedure :: p => procChild
    end type

    contains

    subroutine procBase (b1)
        class(base(4)), intent(in) :: b1
    end subroutine

    subroutine procChild (b1)
        class(child(8)), intent(in) :: b1   !<-- illegal
    end subroutine
end module

end
