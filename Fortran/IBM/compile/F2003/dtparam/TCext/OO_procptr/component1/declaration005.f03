! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/declaration005.f
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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
    type Base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        private
        procedure(integer), nopass, pointer :: pp1
        procedure(type(Base(4,20))), nopass, pointer :: pp2
    end type

    type(Base(4,20)) :: b1
end module

program declaration005
use m
    b1 = Base(4,20)(null(), null())
end
