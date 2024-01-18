!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc016a.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 10/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (SAVE attribute on allocatable
!                               variables)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        INTEGER(8) :: id
    end type

    type, extends(base) :: child
        character(15) :: name
    end type
end module

subroutine test1
use m
    type (base), allocatable, save :: b1, b2(:)
    class (base), allocatable, save :: b3, b4(:)

    integer(4), save :: timesVisited = 0


    if (allocated (b1))  deallocate (b1)
    if (allocated (b2))  deallocate (b2)
    if (allocated (b3))  deallocate (b3)
    if (allocated (b4))  deallocate (b4)


    if (mod (timesVisited,2) == 0) then
        allocate(b1, b2(timesVisited))
        allocate(child :: b3, b4(timesVisited))
    end if
end subroutine

program falloc016a
    do i = 1, 5
        call test1
    end do
end
