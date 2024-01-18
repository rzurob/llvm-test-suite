!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn029a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly pointer array
!*                               assigned to an array section which is composed
!*                               of the parent components of the target)
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

        procedure :: reinitialize => initializeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: reinitialize => initializeChild
    end type

    contains

    subroutine initializeBase (b)
        class (base), intent(out) :: b

        b%id = 0
    end subroutine

    subroutine initializeChild (b)
        class (child), intent(out) :: b

        call b%base%reinitialize

        b%name = ''
    end subroutine
end module

program fpAssgn029a
use m
    class (base), pointer :: b_ptr (:)

    type(child), target :: c1 (2:10)

    c1 = (/(child(i, 'c1'), i=2,10)/)

    b_ptr => c1(2::2)%base

    do i = 1, 5
        call b_ptr(i)%reinitialize
    end do

    do i = 3, 10, 2
        if ((c1(i)%id /= i) .or. (c1(i)%name /= 'c1')) error stop 1_4
    end do

    do i = 2, 10, 2
        if (c1(i)%id /= 0) error stop 2_4
        if (c1(i)%name /= 'c1') error stop 3_4
    end do
end
