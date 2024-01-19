!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
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
    contains

    integer elemental function func1(i)
        integer, intent(in) :: i
        func1 = i
    end function
end module

program declaration006
use m
    procedure(integer), pointer :: p
    p => func1
end
