!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet004.f
! %VERIFY: ffuncRet004.out:ffuncRet004.vf
! %STDIN:
! %STDOUT: ffuncRet004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : poly-function-return (nonpoly pointer
!                               function return in pointer assignment)
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
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        integer*4 :: id

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'empty type'
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id
    end subroutine

    type (child) function produceChildPtr (id)
        pointer produceChildPtr
        integer*4, intent(in) :: id

        allocate (produceChildPtr)

        produceChildPtr%id = id
    end function
end module

program ffuncRet004
use m
    class (base), pointer :: b

    b => produceChildPtr (10)

    call b%print

    deallocate (b)
end
