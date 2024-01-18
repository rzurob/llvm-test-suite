!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd500a2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
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
    type base
        integer(4) id

        contains

        procedure :: getID => getBaseID
        procedure :: setID => setBaseID
    end type

    interface
        elemental subroutine setBaseID (b, id)
        import base
            class (base), intent(inout) :: b
            integer(4), intent (in) :: id
        end subroutine

        elemental integer(4) function getBaseID (b)
        import base
            class (base), intent(in) :: b
        end function
    end interface
end module

elemental subroutine setBaseID (b, id)
use m, only : base
    class (base), intent(inout) :: b
    integer(4), intent (in) :: id

    b%id = id
end

elemental integer(4) function getBaseID (b)
use m, only : base
    class (base), intent(in) :: b

    getBaseID = b%id
end function

program ftpbnd500a2
use m
    type, extends (base) :: child
        character (20) :: name
    end type

    class (base), allocatable :: b1, b2(:)

    !! test b2
    allocate (b2(0:1), source=(/child(1, 'b2 00'), child(2, 'b2 01')/))

    call b2%setID ((/100, 200/))

    if (any(b2%getID() /= (/100, 200/))) error stop 4_4

    select type (b2)
        type is (child)
            if (any(b2%name /= (/'b2 00', 'b2 01'/))) error stop 5_4
        class default
            error stop 6_4
    end select
end
