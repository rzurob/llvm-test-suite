!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg004a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly dummy-arg
!                               to be associated with unlimited poly actual-arg)
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
    contains

    subroutine test1 (x)
        class (*), intent(in), target :: x

        type seq1
            sequence
            integer*4 i1
            integer*8 i2
        end type

        type (seq1), pointer :: s1

        s1 => x

        s1%i1 = s1%i1 * 2
        s1%i2 = s1%i2 ** 3
    end subroutine
end module

program fArg004a
use m
    type seq1
        sequence
        integer*4 i1
        integer*8 i2
    end type

    class (*), allocatable, target :: x1
    type (seq1), pointer :: s1

    !! the next 2 statement tests the allocate syntax
    allocate (x1, source=1_4)

    deallocate (x1)

    allocate (x1, source=seq1(1_4,2_8))

    call test1 (x1)

    s1 => x1

    if ((s1%i1 /= 2) .or. (s1%i2 /= 8)) error stop 1_4
end
