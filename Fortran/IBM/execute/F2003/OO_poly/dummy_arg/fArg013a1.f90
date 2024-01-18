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
! %GROUP: fArg013a1.f
! %VERIFY: fArg013a1.out:fArg013a1.vf
! %STDIN:
! %STDOUT: fArg013a1.out
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
!*  DATE                       : 05/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (TARGET attributes with
!                               both dummy-arg and actual-arg; still scalars)
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

    class (base), pointer :: b_ptr, b_ptr2

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine createb1b2 (b1, b2)
        class (base), pointer, intent(out) :: b1
        class (base), target, intent(out), allocatable :: b2

        if (allocated (b2)) error stop 100_4

        allocate (b2)

        b1 => b2
        b_ptr2 => b2
    end subroutine

    subroutine createb1b2_v (b1, b2)
        class (base), pointer, intent(out) :: b1
        class (base), target, intent(out) :: b2

        b2%id = 10

        b1 => b2
    end subroutine
end module

program fArg013a1
use m
    class (base), allocatable, target :: b1

    allocate (b1, source=child (1, 'b1'))

    call createb1b2 (b_ptr, b1)

    if ((.not. associated (b_ptr, b1)) .or. (.not. associated (b_ptr, b_ptr2)))&
        error stop 1_4

    call b1%print

    deallocate (b1)

    allocate (b1, source=child (1, 'b1'))

    b_ptr => b1

    call createb1b2_v (b_ptr2, b1)

    call b1%print

    if (.not. associated (b_ptr2, b1)) error stop 2_4

    if (.not. associated (b_ptr, b_ptr2)) error stop 3_4

    call b_ptr%print
end
