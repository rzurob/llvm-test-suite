! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/declaration008.f
! opt variations: -qnok -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Duplicate the PUBLIC attribute.
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        private
        procedure(integer), public, nopass, public, pointer :: pp1
        procedure(), private, nopass, private, pointer :: pp2
    end type
end module

program declaration008
use m
end