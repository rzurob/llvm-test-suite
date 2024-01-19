! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/OO_tpbnd/specific/ftpbnd500a2.f
! opt variations: -qnock -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (C458: use the external
!                               procedures as type bound; elemental procedures)
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

        contains

        procedure :: getID => getBaseID
        procedure :: setID => setBaseID
    end type

    interface
        elemental subroutine setBaseID (b, id)
        import base
            class (base(4)), intent(inout) :: b
            integer(4), intent (in) :: id
        end subroutine

        elemental integer(4) function getBaseID (b)
        import base
            class (base(4)), intent(in) :: b
        end function
    end interface
end module

elemental subroutine setBaseID (b, id)
use m, only : base
    class (base(4)), intent(inout) :: b
    integer(4), intent (in) :: id

    b%id = id
end

elemental integer(4) function getBaseID (b)
use m, only : base
    class (base(4)), intent(in) :: b

    getBaseID = b%id
end function

program ftpbnd500a2
use m
    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    class (base(4)), allocatable :: b1, b2(:)

    !! test b2
    allocate (b2(0:1), source=(/child(4,1,20)(1, 'b2 00'), child(4,1,20)(2, 'b2 01')/))

    call b2%setID ((/100, 200/))

    if (any(b2%getID() /= (/100, 200/))) error stop 4_4

    select type (b2)
        type is (child(4,1,*))
            if (any(b2%name /= (/'b2 00', 'b2 01'/))) error stop 5_4
        class default
            error stop 6_4
    end select
end
