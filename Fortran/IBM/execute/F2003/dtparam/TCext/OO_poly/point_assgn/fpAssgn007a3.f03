! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn007a3.f
! opt variations: -qck -qnok -ql -qdefaultpv

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (dissociated pointers have
!*                               dynamic types as declared; try subprograms)
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
        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

module m1
use m
    type container1(k2)    ! (4)
        integer, kind            :: k2
        class(base(k2)), pointer :: data => null()
    end type

    type container2(k3)    ! (4)
        integer, kind           :: k3
        type(base(k3)), pointer :: data => null()
    end type
end module

program fpAssgn007a3
use m
use m1

    interface test
        subroutine testContainer1 (c1, c2)
        use m1
            class (container1(4)), intent(inout) :: c1, c2
        end subroutine

        subroutine testContainer2 (c1, c2)
        use m1
            class (container2(4)), intent(inout) :: c1, c2
        end subroutine
    end interface

    type (container1(4)) :: co1, co2

    type (container2(4)) :: co11, co12

    call test (co1, co2)

    call test (co11, co12)

    call testAgain (co1, co2)
end

subroutine testContainer1 (c1, c2)
use m1
    class (container1(4)), intent(inout) :: c1, c2

    type (child(4,20)), target :: c_obj

    !! initially both pointers are disassociated
    if ((c1%data%typeID() /= 1) .or. (c2%data%typeID() /= 1)) error stop 1_4

    c2%data => c_obj

    if (c2%data%typeID() /= 2) error stop 2_4

    nullify (c2%data)

    if (c2%data%typeID() /= 1) error stop 3_4
end subroutine

subroutine testAgain (c1, c2)
use m1
    type (container1(4)), intent(inout) :: c1, c2

    !! initially both pointers are disassociated
    if ((c1%data%typeID() /= 1) .or. (c2%data%typeID() /= 1)) error stop 5_4

    allocate (child(4,20) :: c2%data)

    if (c2%data%typeID() /= 2) error stop 6_4

    deallocate (c2%data)

    if (c2%data%typeID() /= 1) error stop 7_4
end subroutine

subroutine testContainer2 (c1, c2)
use m1
    class (container2(4)), intent(inout) :: c1, c2

    type (child(4,20)), target :: c_obj

    if ((c1%data%typeID() /= 1) .or. (c2%data%typeID() /= 1)) error stop 10_4

    c2%data => c_obj%base

    if (c2%data%typeID() /= 1) error stop 11_4
end subroutine

