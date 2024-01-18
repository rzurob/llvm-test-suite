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
!                              statement, but overwrite it using
!                              the PUBLIC access attribute.
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
        procedure(integer), public, nopass, pointer :: pp1
        procedure(type(Base)), public, nopass, pointer :: pp2
    end type
end module

program declaration004
use m
    type(Base) :: b1
    b1%pp1 => null()
    b1%pp2 => null()
end
