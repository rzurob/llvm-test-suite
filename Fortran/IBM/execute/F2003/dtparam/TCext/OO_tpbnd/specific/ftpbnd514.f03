! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd514.f
! opt variations: -qck -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (non_overridable bindings
!*                               use in program; with PRIVATE components)
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
        integer, kind        :: k1
        integer(k1), private :: id

        contains

        procedure, non_overridable :: assgnID => assgnID2Base
        procedure, non_overridable :: getID => getBaseID
        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (4,2)
        integer, kind :: k2
        logical(k2)   :: flag

        contains

        procedure :: print => printChild
    end type

    contains

    integer*4 function getBaseID (b)
        class (base(4)), intent(in) :: b

        getBaseID = b%id
    end function

    subroutine assgnID2Base (b, i)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%getID()
    end subroutine

    subroutine printChild (b)
        class (child(4,2)), intent(in) :: b

        print *, b%getID(), b%flag
    end subroutine
end module

module m1
use m, only : child

    type, extends(child) :: gen3(n1)    ! (4,2,20)
        integer, len           :: n1
        character(n1), private :: name

        contains

        procedure, non_overridable :: getName => getGen3Name
        procedure, non_overridable :: assgnName => assgnGen3Name
        procedure :: print => prinGen3
    end type

    contains

    character(20) function getGen3Name (g)
        class (gen3(4,2,*)), intent(in) :: g

        getGen3Name = g%name
    end function

    subroutine assgnGen3Name (g, c)
        class (gen3(4,2,*)), intent(inout) :: g
        character(*), intent(in) :: c

        g%name = c
    end subroutine

    subroutine prinGen3 (b)
        class (gen3(4,2,*)), intent(in) :: b

        print *, b%getID(), b%flag, b%getName()
    end subroutine
end module

program ftpbnd514
use m
use m1, only : gen3

    class (base(4)), pointer :: b_ptr
    type (child(4,2)), target :: c1
    type (gen3(4,2,20)), target :: g1

    call c1%assgnID (10)
    c1%flag = .false.

    call g1%assgnID (20)
    call g1%assgnName ('g1')
    g1%flag = (1< 10)

    if (c1%getID() /= 10) error stop 1_4

    if (g1%getID() /= 20) error stop 2_4

    if (g1%getName() /= 'g1') error stop 3_4

    call c1%print
    call g1%print
    call g1%child%base%print

    b_ptr => g1

    if (b_ptr%getID() /= 20) error stop 4_4

    call b_ptr%print
end