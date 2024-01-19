! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg010a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; pointer
!*                               component won't be changed if re-allocated)
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
        integer(k1)   :: id = -1

        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure, nopass :: typeID => childID
    end type

    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()
    end type

    contains

    subroutine allocateTemp (c)
        type (container(4)), value :: c

        allocate (child(4,1,20):: c%data)

        if (c%data%typeID () /= 2) error stop 10_4
    end subroutine

    integer*4 function baseID()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

program fArg010a1
use m
    class (container(4)), allocatable :: c1 (:)

    allocate (c1 (3))

    call allocateTemp (c1(1))

    allocate (c1(2)%data)

    call allocateTemp (c1(2))

    if (associated (c1(1)%data) .or. associated (c1(3)%data)) error stop 1_4

    if (.not. associated (c1(2)%data)) error stop 2_4

    if (c1(2)%data%typeID () /= 1) error stop 3_4

    deallocate (c1(2)%data)
end
