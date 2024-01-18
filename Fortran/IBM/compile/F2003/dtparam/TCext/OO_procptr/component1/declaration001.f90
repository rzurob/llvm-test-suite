! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/component1/declaration001.f
! opt variations: -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
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
        procedure(integer), nopass :: pp1 => null()
        procedure(type(Base(20,4))), nopass :: pp2 => null()
    end type
end module

program declaration001
use m
end
