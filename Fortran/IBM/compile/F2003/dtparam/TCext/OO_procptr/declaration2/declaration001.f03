! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_procptr/declaration2/declaration001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The procedure is a dummy argument and has
!                              the INTENT attribute. Then it must be
!                              declared with the POINTER attribute.
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

    subroutine sub1(b, p)
        class(Base(*,4)), pointer, intent(in) :: b
        procedure(integer), intent(in) :: p
        print *, "sub1"
    end subroutine

    integer function func1(b, p)
        class(*), allocatable, intent(in) :: b
        procedure(integer), intent(in) :: p
        func1 = 10
    end function
end module

program declaration001
end
