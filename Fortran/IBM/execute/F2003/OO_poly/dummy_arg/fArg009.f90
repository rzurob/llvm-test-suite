!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg009.f
! %VERIFY: fArg009.out:fArg009.vf
! %STDIN:
! %STDOUT: fArg009.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (If the dummy-arg has the
!                               VALUE attribute it becomes associated with a
!                               definable anonymous data whose initial value is
!                               that of the actual argument.)
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
        integer*4, pointer :: id => null()

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (associated (b%id)) then
            print *, b%id
        else
            print *, 'null'
        end if
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        call b%base%print
        print *, b%name
    end subroutine

    subroutine printData (b)
        type (base), value :: b

        call b%print
    end subroutine
end module

program fArg009
use m
    class (base), allocatable :: b1, b2
    integer*4, target :: i1 = 10

    allocate (b1, source=child(null(), 'b1'))
    allocate (b2, source = child (i1, 'b2'))

    call printData (b1)

    call printData (b2)
end
