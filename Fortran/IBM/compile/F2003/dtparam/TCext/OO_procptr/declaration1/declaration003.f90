! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/declaration003.f
! opt variations: -qnol

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type
end module

program declaration003
use m
    procedure(integer) :: pp1 => null()
    procedure(type(Base(20,4))) :: pp2 => null()
end
