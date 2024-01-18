! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn007.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends(base) :: child(k2)    ! (4,20,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name

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

    class(base(4,:)), pointer :: b_ptr   !! initially undefined

    type (base(4,20)), target :: b1
    type (child(4,20,1)), target :: c1
    class (child(4,:,1)), pointer :: c_ptr

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
