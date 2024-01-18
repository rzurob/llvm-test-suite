!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn022.f
! %VERIFY: fpAssgn022.out:fpAssgn022.vf
! %STDIN:
! %STDOUT: fpAssgn022.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (same module pointer is
!*                               renamed in a separate module and both names can
!*                               be accessible)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
    end type

    class (base), pointer :: b1_m => null()

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

end module

module m1
use m, b1 => b1_m

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine setB1 (i)
        integer*4, intent(in) :: i

        allocate (b1)

        b1%id = i
    end subroutine
end module

program fpAssgn022
use m
use m1
    !! b1_m and b1 are the two names for the same entity
    type (child), target :: c1

    c1 = child (1, 'c1')

    if (associated (b1)) error stop 1_4

    b1 => c1

    if (.not. associated (b1_m, c1)) error stop 2_4

    call b1_m%print

    allocate (b1_m)

    b1_m%id = 100

    call b1%print

    deallocate (b1_m)

    if (associated (b1)) error stop 3_4

    call setB1 (-10)

    if (b1_m%id /= -10) error stop 4_4

    deallocate (b1)

    if (associated (b1_m)) error stop 5_4
end
