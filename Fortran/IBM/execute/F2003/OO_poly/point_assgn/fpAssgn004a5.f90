!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn004a5.f
! %VERIFY: fpAssgn004a5.out:fpAssgn004a5.vf
! %STDIN:
! %STDOUT: fpAssgn004a5.out
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

        procedure :: duplicate => produceBasePtr
        procedure :: print => printBase
    end type

    contains

    function produceBasePtr (b)
        type (base), pointer :: produceBasePtr
        class (base), intent(in) :: b

        allocate (produceBasePtr)

        produceBasePtr%id = b%id
    end function

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fpAssgn004a5
use m
    type (base), target :: b1(10)

    type (base), pointer :: b

    class (base), pointer :: b_ptr

    b1 = (/(base(i), i=1,10)/)

    b => b1(5)%duplicate()

    call b%print

    deallocate (b)

    b_ptr => b1(3)%duplicate()

    call b_ptr%print

    deallocate (b_ptr)
end
