! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/class/fclass001a.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (unallocated poly-allocatable
!*                               and disassociated poly-pointers have dynamic
!*                               types of the declared types)
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
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

module m1
use m, only : base, child
    type dataType1(k2,n2)    ! (4,20)
        integer, kind                  :: k2
        integer, len                   :: n2
        class(base(k2,:)), allocatable :: data
    end type

    type dataType2(k3,n3)    ! (4,20)
        integer, kind              :: k3
        integer, len               :: n3
        class(base(k3,:)), pointer :: data
    end type

    type dataType3(k4,n4)    ! (4,20)
        integer, kind               :: k4
        integer, len                :: n4
        class(child(k4,:)), pointer :: data => null()
    end type

    type (dataType1(4,20)) :: d1_m
    type (dataType2(4,20)) :: d2_m = dataType2(4,20) (null())
    type (dataType3(4,20)), save :: d3_m
end module


program fclass001a
use m1
    if (d1_m%data%typeID () /= 1) error stop 1_4

    if (d2_m%data%typeID () /= 1) error stop 2_4

    if (d3_m%data%typeID () /= 2) error stop 3_4
end
