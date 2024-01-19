!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The procedure declaration has the =>
!                              initialization part, then the POINTER
!                              attribute must appear.
!
!                              This test case is diagnostic. But the
!                              current implementation does not fail
!                              the test case; instead it assumes the
!                              POINTER attribute. So the VALIDRCS in
!                              the scenariio file should be 0 instead
!                              of 1.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type
end module

program declaration003
use m
    procedure(integer) :: pp1 => null()
    procedure(type(Base)) :: pp2 => null()
end
