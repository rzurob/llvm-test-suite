! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_procptr/component2/declaration001.f
! opt variations: -qnok -qnol -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/component2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    contains

    type(Base(20,4)) elemental function func1(b)
        class(Base(*,4)), intent(in) :: b
        func1 = Base(20,4)(b%i)
    end function
end module

program declaration001
use m
    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(type(Base(20,4))), pointer, nopass :: p
    end type

    type(Container(4,20)) :: c1
    c1%p => func1
end
