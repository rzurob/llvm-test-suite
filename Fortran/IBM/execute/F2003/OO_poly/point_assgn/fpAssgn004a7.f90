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
! %GROUP: fpAssgn004a7.f
! %VERIFY: fpAssgn004a7.out:fpAssgn004a7.vf
! %STDIN:
! %STDOUT: fpAssgn004a7.out
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
!*  DATE                       : 03/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (pointer assigned to
!*                               type bound functions; nopass binding)
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

        procedure, nopass :: replicateBase
        procedure :: print => printBase
    end type

    class (base), pointer :: b1_m
    private replicateBase, printBase

    contains

    function replicateBase (b)
        type (base), pointer :: replicateBase
        type (base), intent(in) :: b

        allocate (replicateBase)

        replicateBase = b
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type, extends(base) :: child
        character*20 :: name =''

        contains

        procedure, nopass :: replicateChild
        procedure :: print => printChild
    end type

    contains

    function replicateChild (c)
        type (child), pointer :: replicateChild
        type (child), intent(in) :: c

        allocate (replicateChild)

        replicateChild = c
    end function

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fpAssgn004a7
use m1
    class (base), pointer :: b_ptr
    type (base) :: b1
    type (child) :: c1

    b1%id = 10

    c1 = child (20, 'c1')

    b1_m => c1%replicateBase (b1)

    call b1_m%print
    deallocate (b1_m)

    b1_m => c1%replicateBase (c1%base)

    call b1_m%print
    deallocate (b1_m)

    b1_m => c1%replicateChild (c1)

    call b1_m%print
    deallocate (b1_m)

    b_ptr => b1%replicateBase (b1)

    call b_ptr%print
    deallocate (b_ptr)
end
