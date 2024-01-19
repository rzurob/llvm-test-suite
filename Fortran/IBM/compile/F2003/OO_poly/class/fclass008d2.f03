! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : CLASS keyword (intrinsic assignment and
!                               default IO can not apply to associate-name
!                               to a poly dummy-arg)
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
        integer*4 :: id = 1
    end type

    contains

    subroutine testAssociate (b)
        class (base), intent(inout) :: b

        associate (x => b)
            x = base (10)       !<-- illegal

            print *, x          !<-- illegal
        end associate
    end subroutine
end module

program fclass008d2
end
