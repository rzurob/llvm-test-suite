! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/declaration009.f
! opt variations: -qnok -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Combine PUBLIC and PRIVATE attributes.
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
    type Base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        private
        procedure(integer), public, nopass, private, pointer :: pp1
        procedure(), private, nopass, public, pointer :: pp2
    end type
end module

program declaration009
use m
end
