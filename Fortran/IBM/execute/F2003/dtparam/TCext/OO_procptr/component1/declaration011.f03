! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/declaration011.f
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Initialize procedure pointer component
!                              using NULL(), and check its initial
!                              association status using ASSOCIATED.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface
        integer function interfaceFunc1()
        end function
    end interface

    type Base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        procedure(interfaceFunc1), nopass, pointer :: pp1 => null()
        procedure(), nopass, pointer :: pp2 => null()
    end type
end module

program declaration011
use m
    type(Base(4,20)) :: b1
    if(associated(b1%pp1)) error stop 1_4
    if(associated(b1%pp2)) error stop 2_4
end
