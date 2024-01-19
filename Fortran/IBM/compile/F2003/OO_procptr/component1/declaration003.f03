!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare the accessibility of procedure
!                              pointer component using PRIVATE
!                              statement.
!
!                              This test case is diagnostic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        private
        procedure(integer), nopass, pointer :: pp1
        procedure(type(Base)), nopass, pointer :: pp2
    end type
end module

program declaration003
use m
    type(Base) :: b1
    b1%pp1 => null()
    b1%pp2 => null()
end
