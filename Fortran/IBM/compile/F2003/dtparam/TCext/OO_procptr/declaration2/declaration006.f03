! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_procptr/declaration2/declaration006.f
! opt variations: -qnol -qreuse=base

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : Assiign the procedure pointer to a
!                              non-intrinsic elemental procedure.
!                              Poly or unlimited poly.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type

    contains

    type(Base(20,4)) elemental function func1(b)
        class(Base(*,4)), intent(in) :: b
        func1 = Base(20,4)(b%i)
    end function
end module

program declaration006
use m
    procedure(type(Base(20,4))), pointer :: p
    p => func1
end
