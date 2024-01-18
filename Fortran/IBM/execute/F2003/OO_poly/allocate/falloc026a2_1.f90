!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc026a2_1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocate of a disassociated pointer
!                               will cuase an error condition)
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
        integer i

        contains

        final final1
    end type

    contains

    subroutine final1 (b)
        type (base) b
    end subroutine
end module

program falloc026a2_1
use m
    class(base), pointer :: b1(:) => null()
    type (base), pointer :: b2 => null()

    class (base), allocatable :: b3
    type (base), allocatable :: b4

    integer err(4)

    deallocate (b1, stat=err(1))
    deallocate (b2, stat=err(2))
    deallocate (b3, stat=err(3))
    deallocate (b4, stat=err(4))

    if (any(err == 0)) error stop 1_4


    !! try to deallocate twice for a pointer
    allocate (b1(20), b2)

    deallocate (b1, b2)

    deallocate (b1, stat=err(1))
    deallocate (b2, stat=err(2))


    !! try to deallocate nullified pointers
    allocate (b1(10), b2)

    nullify (b1, b2)

    deallocate (b1, stat=err(3))
    deallocate (b2, stat=err(4))

    if (any(err == 0)) error stop 2_4
end
