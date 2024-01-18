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
! %GROUP: fpAssgn003a7.f
! %VERIFY: fpAssgn003a7.out:fpAssgn003a7.vf
! %STDIN:
! %STDOUT: fpAssgn003a7.out
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
!*  DATE                       : 03/19/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
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

    type (base), pointer :: b(:)

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
        class (base), intent(in) :: b(:)

        do i = 1, size(b)
            call b(i)%print
        end do
    end subroutine
end module

program fpAssgn003a6
use m, only : base, child, printData, b

    class (base), pointer :: b_ptr(:)
    class (child), pointer :: c1 (:)

    allocate (c1(10))

    c1%id = (/(i, i=1,10)/)
    c1%name = 'c1'

    b => c1(::3)%base

    if (size(b) /= 4) error stop 1_4

    call printData (b)
    call printData (c1(::3))

    b_ptr => c1(::4)

    call printData (b_ptr)

    b => b_ptr

    if (size(b) /= 3) error stop 2_4

    call printData (b)

    deallocate (c1)
end
