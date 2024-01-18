!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn004a9.f
! %VERIFY: fpAssgn004a9.out:fpAssgn004a9.vf
! %STDIN:
! %STDOUT: fpAssgn004a9.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data poiner assignment (poly-pointer assigned
!*                               to function return non-poly pointer; caller is
!*                               a poly-pointer)
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
        integer*4 :: id = 0

        contains

        procedure :: replicate => produceBasePtr
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*15 :: name = 'no-name'

        contains

        procedure :: print => printChild
    end type

    contains

    function produceBasePtr (b)
        type (base), pointer :: produceBasePtr
        class (base), intent(in) :: b

        allocate (produceBasePtr)

        produceBasePtr%id = b%id
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn004a9
use m, only : base, child
    type (child), target :: c1

    class (base), pointer :: b_ptr


    c1 = child (10, 'c1')

    b_ptr => c1

    b_ptr => b_ptr%replicate()

    call b_ptr%print

    deallocate (b_ptr)

end
