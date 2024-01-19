!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/03/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : Diagnose test case. If the context of the
!                              reference to null is an actual argument
!                              to a generic procedure, MOLD shall be
!                              present if the type, type parameters, or
!                              rank is required to resolve the generic
!                              reference.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    interface gen
        subroutine s1(j, pi)
            integer j
            integer, pointer :: pi
        end subroutine

        subroutine s2(k, pr)
            integer k
            real, pointer :: pr
        end subroutine
    end interface gen
end module

program generic001
use m
    real, pointer :: real_ptr
    call gen(7, null())
end

subroutine s1(j, pi)
    integer j
    integer, pointer :: pi
    print *, "a"
end subroutine

subroutine s2(k, pr)
    integer k
    integer, pointer :: pr
    print *, "b"
end subroutine
