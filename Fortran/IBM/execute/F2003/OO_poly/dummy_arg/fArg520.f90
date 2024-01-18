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
!*  DATE                       : 01/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (undefinable actual-args
!                               associated with unlimited poly assumed-shape
!                               array)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine printSize (x)
        class (*), intent(in) :: x(:)

        print *, size (x)
    end subroutine
end module

program fArg520
use m
    integer, parameter :: i_c(3) = (/1,2,3/)

    class (*), pointer :: x1(:)

    allocate (integer :: x1(0))

    call printSize ((/10/))

    call printSize (x1)

    call printsize (i_c(::2))
end
