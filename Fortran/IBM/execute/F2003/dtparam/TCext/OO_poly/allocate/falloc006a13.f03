! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/allocate/falloc006a13.f
! opt variations: -qck -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : allocate (source-expr is a call to elemental
!                               function call that returns derived type;
!                               allocate for an unlimited poly-allocatable array)
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
        integer(k1)      id
    end type

    type, extends(base) :: child(n1)    ! (4,18)
        integer, len  :: n1
        character(n1)    name
    end type

    interface makeData
        module procedure createBase, createChild
    end interface

    contains

    elemental type (base(4)) function createBase (id)
        integer, intent(in) :: id

        createBase%id = id
    end function

    elemental type (child(4,18)) function createChild (id, name)
        integer, intent(in) :: id
        character(*), intent(in) :: name

        createChild%id = id
        createChild%name = name
    end function
end module

program falloc006a13
use m
    class(*), allocatable :: x1(:)

    allocate (x1(3), source=makeData ((/1,2,3/), (/'xlftest 1', 'xlftest 2', &
                        'xlftest 3'/)))

    select type (x1)
        type is (child(4,*))
            if (any(x1%id /= (/1,2,3/))) error stop 1_4

            if (any (x1%name /= (/'xlftest 1', 'xlftest 2', 'xlftest 3'/))) &
                error stop 2_4
        class default
            error stop 4_4
    end select
end
