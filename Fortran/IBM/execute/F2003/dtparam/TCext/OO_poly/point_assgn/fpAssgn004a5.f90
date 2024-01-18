! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn004a5.f
! opt variations: -ql

! SCCS ID Information
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: duplicate => produceBasePtr
        procedure :: print => printBase
    end type

    contains

    function produceBasePtr (b)
        type (base(4)), pointer :: produceBasePtr
        class (base(4)), intent(in) :: b

        allocate (produceBasePtr)

        produceBasePtr%id = b%id
    end function

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fpAssgn004a5
use m
    type (base(4)), target :: b1(10)

    type (base(4)), pointer :: b

    class (base(4)), pointer :: b_ptr

    b1 = (/(base(4)(i), i=1,10)/)

    b => b1(5)%duplicate()

    call b%print

    deallocate (b)

    b_ptr => b1(3)%duplicate()

    call b_ptr%print

    deallocate (b_ptr)
end
