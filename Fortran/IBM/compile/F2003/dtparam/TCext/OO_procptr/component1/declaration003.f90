! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/declaration003.f
! opt variations: -qnok -qnol

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
    type Base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        private
        procedure(integer), nopass, pointer :: pp1
    end type
end module

program declaration003
use m
    type(Base(4,20)) :: b1
    b1%pp1 => null()
end
