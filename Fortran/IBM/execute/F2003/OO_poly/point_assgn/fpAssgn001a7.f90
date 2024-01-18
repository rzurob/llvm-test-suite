!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn001a7.f
! %VERIFY: fpAssgn001a7.out:fpAssgn001a7.vf
! %STDIN:
! %STDOUT: fpAssgn001a7.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (base poly pointer
!*                               points to multi-children types)
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
        integer, pointer :: flag
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child1
        integer*4 :: id

        contains

        procedure :: print => printChild1
    end type

    type, extends(base) :: child2
        character*20 :: name

        contains

        procedure :: print => printChild2
    end type

    class(base), pointer :: b1_m
    class (child1), allocatable, target :: c1_m
    type (child2), target :: c2_m

    contains

    subroutine printBase(b)
        class (base), intent(in) :: b
        print *, 'type base'
    end subroutine

    subroutine printChild1 (b)
        class (child1), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild2 (b)
        class (child2), intent(in) :: b

        print *, b%name
    end subroutine
end module

program fpAssgn001a7
use m
    class (base), pointer :: b_ptr

    type (child1), target :: c1
    type (child2), target :: c2

    c1%id = 10
    c2%name = 'c2'

    b_ptr => c1

    call b_ptr%print

    b_ptr => c2

    call b_ptr%print

    c2_m%name = 'c2_m'

    b_ptr => c2_m
    call b_ptr%print


    allocate (c1_m)
    c1_m%id = -1
    b_ptr => c1_m%base

    call b_ptr%print

    b_ptr => c1_m
    b1_m => b_ptr
    call b1_m%print
end
