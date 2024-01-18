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
! %GROUP: fpAssgn011.f
! %VERIFY: fpAssgn011.out:fpAssgn011.vf
! %STDIN:
! %STDOUT: fpAssgn011.out
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
!*  DATE                       : 02/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : pointer assignment (self assignment of
!*                               poly-pointers, test the target association
!*                               status)
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

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name
        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'id = ', b%id, '; name = ', b%name
    end subroutine
end module

program fpAssgn011
use m

    class (child), pointer :: c1_ptr
    class (base), pointer :: b_ptr

    type (child), target, allocatable :: c_alloc

    b_ptr => null()

    b_ptr => b_ptr

    if (associated(b_ptr)) error stop 1_4

    allocate (c1_ptr, c_alloc)

    c1_ptr%id = 1
    c1_ptr%name = 'c1_ptr'

    b_ptr => c1_ptr

    call b_ptr%print

    b_ptr => b_ptr

    if (.not. associated (b_ptr, c1_ptr)) error stop 2_4

    call b_ptr%print

    deallocate (b_ptr)

    c_alloc = child (2, 'c_alloc')

    b_ptr => c_alloc

    call b_ptr%print

    b_ptr => b_ptr

    if (.not. associated (b_ptr, c_alloc)) error stop 3_4
    call b_ptr%print
end
