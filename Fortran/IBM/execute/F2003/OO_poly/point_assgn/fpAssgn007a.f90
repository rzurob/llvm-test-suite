!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn007a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (disassociated pointers
!*                               having dynamic types of their declared)
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

    type, extends(child) :: grandP
        integer*4 :: id

        contains

        procedure, nopass :: typeID => grandPID
        procedure :: print => printGrandP
    end type

    contains

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function

    integer*4 function grandPID ()
        grandPID = 3
    end function

    subroutine printGrandP (g)
        class (grandP), intent(in) :: g

        print *, 'name =', g%name, '; id = ', g%id
    end subroutine
end module

program fpAssgn007a
use m

    type(base), pointer :: b_ptr1
    type (child), pointer :: c_ptr1
    type (grandP), pointer :: g_ptr1

    class (base), pointer :: b_ptr2
    class (child), pointer :: c_ptr2
    class (grandP), pointer :: g_ptr2

    !! nonpolymorphic pointers' types are their declared type regardless
    !! the association status
    if ((b_ptr1%typeID() /= 1) .or. (c_ptr1%typeID() /= 2) .or. &
        (g_ptr1%typeID() /= 3)) error stop 1_4

    !! disassociated poly-pointers' dynamic types are of their decalred types
    b_ptr2 => null()
    c_ptr2 => null()
    g_ptr2 => null()

    if ((b_ptr2%typeID() /= 1) .or. (c_ptr2%typeID() /= 2) .or. &
        (g_ptr2%typeID() /= 3)) error stop 2_4


    allocate (b_ptr1, c_ptr1, g_ptr1)

    c_ptr2 => g_ptr1
    b_ptr2 => c_ptr2

    if ((b_ptr2%typeID() /= 3) .or. (c_ptr2%typeID() /= 3)) error stop 3_4

    deallocate (g_ptr1)

    c_ptr2 => g_ptr1
    b_ptr2 => c_ptr2

    if ((b_ptr2%typeID() /= 1) .or. (c_ptr2%typeID() /= 2)) error stop 4_4

    deallocate (b_ptr1, c_ptr1)
end
