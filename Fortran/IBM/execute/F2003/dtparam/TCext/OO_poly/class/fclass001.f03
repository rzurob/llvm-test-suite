! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass001.f

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (unallocated allocatable has a
!*                               dynamic type of its declared type)
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
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

program fclass001
use m

    class (base(4,:)), allocatable :: b_allo

    if (b_allo%typeID() /= 1) error stop 1_4

    allocate (child(4,20,1):: b_allo)

    if (b_allo%typeID() /= 2) error stop 2_4

    deallocate (b_allo)

    if (b_allo%typeID() /= 1) error stop 3_4
end
