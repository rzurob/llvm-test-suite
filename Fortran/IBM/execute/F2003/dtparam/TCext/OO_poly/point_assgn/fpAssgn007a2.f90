! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn007a2.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn007a2.f
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
!*                               to verify the dynamic type; pointer component)
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

    type, extends(base) :: child    ! (4,20)
        character(n1) :: name

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

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind               :: k2
        integer, len                :: n2
        class(base(k2,n2)), pointer :: data => null()
    end type
end module


program fpAssgn007a2
use m           !! this is redundant
use m1

    interface assignment (=)
        subroutine containerAssgn (c1, c2)
        use m1
            class (container(4,*)), intent(out) :: c1
            class (container(4,*)), intent(in) :: c2
        end subroutine
    end interface

    type (container(4,20)) :: co1
    type (child(4,20)), target :: c1
    class (container(4,20)), pointer :: co_ptr

    allocate (co_ptr)

    co_ptr%data => c1

    if (co_ptr%data%typeID() /= 1) error stop 10_4

    co_ptr = co1

    if (associated(co_ptr%data)) error stop 11_4

    if (co_ptr%data%typeID() /= 0) error stop 12_4

    co1%data => c1

    if (co1%data%typeID() /= 1) error stop 13_4

    nullify (co1%data)

    if (co1%data%typeID() /= 0) error stop 14_4

    deallocate (co_ptr)
end

subroutine containerAssgn (c1, c2)
use m1
    class (container(4,*)), intent(out) :: c1
    class (container(4,*)), intent(in) :: c2

    if (associated (c1%data)) error stop 1_4

    if (c1%data%typeID() /= 0) error stop 2_4

    c1%data => c2%data
end subroutine
