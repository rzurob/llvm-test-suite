!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc504a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE/DEALLOCATE (deallocating a pointer
!                               whose target is not created by ALLOCATE
!                               statement will result in an error message)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type
end module

program falloc504a
use m
    class (base), pointer :: b1
    type (child), pointer :: c1

    class (*), pointer :: x
    integer(4) error

    allocate (c1)

    b1 => c1%base

    x => c1%name

    error = 0

    deallocate (b1, stat=error)

    if (error /= 2) error stop 1_4

    error = 0

    deallocate (x, stat=error)

    if (error /= 2) error stop 2_4

    error = 0

    x => c1%base

    error = 0
    deallocate (x, stat=error)

    if (error /= 2) error stop 3_4

    x => b1
    error = 0

    deallocate (x, stat=error)

    if (error /= 2) error stop 4_4

    x => c1

    deallocate (x, stat=error)

    if (error /= 0) error stop 5_4
end
