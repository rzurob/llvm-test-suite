! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/declaration002.f
! opt variations: -qnok -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Assiign the procedure pointer to a
!                              non-intrinsic elemental procedure.
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
        procedure(integer), nopass, pointer :: p
    end type

    contains

    integer elemental function func1(i)
        integer, intent(in) :: i
        func1 = i
    end function
end module

program declaration002
use m
    type(Base(4)) :: b1
    b1%p => func1
end
