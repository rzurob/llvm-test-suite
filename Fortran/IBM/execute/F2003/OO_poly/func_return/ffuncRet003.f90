!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet003.f
! %VERIFY: ffuncRet003.out:ffuncRet003.vf
! %STDIN:
! %STDOUT: ffuncRet003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/117/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (pointer dummy-arg as the
!                               returned pointer)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    class (base) function updateVal (b, id)
        class (base), intent(inout), pointer :: b
        pointer updateVal
        integer*4, intent(in) :: id

        b%id = id
        updateVal => b
    end function
end module

program ffuncRet003
use m
    class (base), pointer :: b1, b2

    type (child), target :: c1

    c1%id = 1
    c1%name = 'c1'

    b1 => c1

    b2 => updateVal (b1, 10)

    call b2%print

    if (.not. associated (b2, b1)) error stop 1_4
end
