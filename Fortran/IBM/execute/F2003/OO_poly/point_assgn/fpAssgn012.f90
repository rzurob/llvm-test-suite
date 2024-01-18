!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn012.f
! %VERIFY: fpAssgn012.out:fpAssgn012.vf
! %STDIN:
! %STDOUT: fpAssgn012.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (elemental binding call
!                               used in WHERE construct)
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
        integer id

        contains

        procedure :: print => printBase
        procedure :: getID => getBaseID
    end type

    private printBase, getBaseID

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    elemental integer function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function
end module

program fpAssgn012
use m

    type (base), pointer :: b_ptr(:)

    type (base), target :: b1(100), b2

    b1 = (/(base(i), i=1,100)/)

    b_ptr => b1

    where (b1%getID() < 20)
        b1%id = 20
    end where

    where (b_ptr%getID() > 50)
        b_ptr%id = 50
    end where

    print *, b1%getID()

    if (any (b_ptr(1:20)%getID() /= 20)) error stop 1_4

    if (any (b_ptr(21:50)%getID() /= (/(i,i=21,50)/))) error stop 2_4

    if (any (b_ptr(51:)%getID() /= 50)) error stop 3_4

end
