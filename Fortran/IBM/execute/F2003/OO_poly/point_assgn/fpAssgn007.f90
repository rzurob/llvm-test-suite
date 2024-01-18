!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn007.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (poly-pointer assignment
!*                               may cause it to become disassociated; use
!*                               null(), nullify statement; use nopass binding
!*                               to verify the dynamic type)
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
        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    integer*4 function baseID()
        baseID = 0
    end function

    integer*4 function childID ()
        childID = 1
    end function
end module

program fpAssgn007
use m

    class(base), pointer :: b_ptr   !! initially undefined

    type (base), target :: b1
    type (child), target :: c1
    class (child), pointer :: c_ptr

    b_ptr => c1
    if (b_ptr%typeID() /= 1) error stop 1_4

    nullify (b_ptr)

    if (associated(b_ptr) .or. (b_ptr%typeID() /= 0)) error stop 2_4

    b_ptr => b1
    if (b_ptr%typeID() /= 0) error stop 3_4

    b_ptr => c1

    b_ptr => null()
    if (associated(b_ptr) .or. (b_ptr%typeID() /= 0)) error stop 4_4


    c_ptr => null()

    b_ptr => c_ptr

    if (associated(b_ptr) .or. (b_ptr%typeID() /= 0)) error stop 5_4
end
