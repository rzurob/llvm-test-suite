!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Declare the accessibility of procedure
!                              pointer component using PRIVATE
!                              statement.
!
!                              This test case is diagnostic. Use
!                              structure constructor to check the
!                              accessibility.
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

    type(Base) :: b1
end module

program declaration005
use m
    b1 = Base(null(), null())
end
