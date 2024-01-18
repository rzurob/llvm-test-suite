!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn003a6.f
! %VERIFY: fpAssgn003a6.out:fpAssgn003a6.vf
! %STDIN:
! %STDOUT: fpAssgn003a6.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (non-poly data pointer
!*                               assigned to poly, or ancestor component of the
!*                               data-target; test its dynamic types using
!*                               type bound in procedure calls)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = ''

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

    subroutine printData (b)
        class (base), intent(in) :: b

        call b%print
    end subroutine
end module

program fpAssgn003a6
use m, only : base, child, printData

    type (base), pointer :: b

    class (base), pointer :: b_ptr
    class (child), pointer :: c1

    allocate (c1)

    c1%id = 1
    c1%name = 'c1'

    b => c1%base

    call printData (b)
    call printData (c1)

    b_ptr => c1

    call printData (b_ptr)

    b => b_ptr

    call printData (b)
    deallocate (c1)
end
