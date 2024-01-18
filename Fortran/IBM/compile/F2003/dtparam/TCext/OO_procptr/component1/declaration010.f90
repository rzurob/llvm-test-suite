! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/declaration010.f
! opt variations: -qnok -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Declare the procedure pointer component
!                              without the POINTER attribute, POINTER
!                              is assumed.
!
!                              This test case is diagnostic but does not
!                              fail because POINTER is assumed.
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
        procedure(integer), nopass :: pp1
        procedure(type(Base(4))), nopass :: pp2
    end type
end module

program declaration010
use m
    type(Base(4)) :: b1
    b1%pp1 => null()
    b1%pp2 => null()
end
