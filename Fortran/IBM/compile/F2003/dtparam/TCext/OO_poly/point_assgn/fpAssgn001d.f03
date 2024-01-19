! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn001d.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2005
!*
!*  DESCRIPTION                : data pointer assignment (C716 type
!                               compatibility between POINTER and TARGET)
!*
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

program fpAssgn001d
use m

    class (base(4,:)), pointer :: b_ptr
    class (child(4,:,1)), pointer :: c_ptr
    class (*), pointer :: x

    integer, target :: i
    type (base(4,20)), target :: b1
    type (child(4,20,1)), target :: c1

    c_ptr => b1         !<-- illegal

    c_ptr => b_ptr      !<-- illegal

    c_ptr => c1%base    !<-- illegal

    c_ptr => x          !<-- illegal

    b_ptr => i          !<-- illegal
end
