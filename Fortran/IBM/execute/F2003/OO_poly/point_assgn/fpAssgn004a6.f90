!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn004a6.f
! %VERIFY: fpAssgn004a6.out:fpAssgn004a6.vf
! %STDIN:
! %STDOUT: fpAssgn004a6.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer assigned to
!*                               function return results; function is
!*                               type-bound)
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

        procedure :: clone => produceBasePtr
        procedure :: print => printBase
    end type

    contains

    function produceBasePtr (b)
        class (base), pointer :: produceBasePtr
        class (base), intent(in) :: b

        allocate (produceBasePtr)

        produceBasePtr%id = b%id
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m, only : base

    type, extends (base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
        procedure :: clone => produceChildPtr
    end type

    contains

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    function produceChildPtr (b)
        class (base), pointer :: produceChildPtr
        class (child), intent(in) :: b

        type (child), pointer :: temp

        allocate (temp)

        temp = child (b%id, b%name)

        produceChildPtr => temp
    end function
end module

program fpAssgn004a6
use m1, only : base, child
    type (base) :: b1
    type (child) :: c1

    class (base), pointer :: b_ptr


    c1 = child (10, 'c1')

    b_ptr => c1%clone()

    call b_ptr%print

    deallocate (b_ptr)

    b1%id = 20

    b_ptr => b1%clone()

    call b_ptr%print

    deallocate (b_ptr)
end
