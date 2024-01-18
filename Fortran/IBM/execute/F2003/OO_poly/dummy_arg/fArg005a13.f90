!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a13.f
! %VERIFY: fArg005a13.out:fArg005a13.vf
! %STDIN:
! %STDOUT: fArg005a13.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (pointer assignment for
!*                               the poly-pointer dummy-arg)
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
        character*20 :: name = 'no-name'

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

    subroutine associate (b, b1)
        class (base), pointer, intent(out) :: b
        class (base), target, intent(in) :: b1

        b => b1
    end subroutine

    subroutine associateArray (b, b1)
        class (base), pointer, intent(out) :: b(:)
        class (base), target, intent(in) :: b1(:)

        b => b1
    end subroutine
end module


program fArg005a13
use m
    class (base), pointer :: b1, b2 (:)

    type (child), target :: c1, c2 (3:5)
    type (child) :: c3 (2)

    class (base), allocatable, target :: b3, b4(:)

    c1 = child (1, 'c1_target')

    call associate (b1, c1)

    if (.not. associated (b1, c1)) error stop 1_4

    call b1%print

    allocate (b3, source=child (2, 'b3_allocatable'))

    call associate (b1, b3)

    if (.not. associated (b1, b3)) error stop 2_4

    call b1%print

    !! done with scalars

    call associateArray (b2, c2)

    if (.not. associated (b2, c2)) error stop 3_4

    if (size (b2) /= 3) error stop 4_4

    if ((lbound(b2,1) /= 1) .or. (ubound(b2,1) /= 3)) error stop 5_4

    c2%id = (/3,4,5/)
    c2%name = (/'c2_target_3', 'c2_target_4', 'c2_target_5'/)

    call b2(1)%print
    call b2(2)%print
    call b2(3)%print

    c3%id = (/6,7/)
    c3%name = (/'b4_allocatable_6', 'b4_allocatable_7'/)

    allocate (b4 (6:7), source=c3)

    if (lbound(b4, 1) /= 6) error stop 6_4

    call associateArray (b2, b4)

    if (size(b2) /= 2) error stop 7_4

    if ((lbound(b2,1) /= 1) .or. (ubound(b2,1) /= 2)) error stop 8_4

    call b2(1)%print
    call b2(2)%print
end
