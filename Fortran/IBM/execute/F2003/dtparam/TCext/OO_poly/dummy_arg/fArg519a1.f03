! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg519a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for unlimited
!                               poly actual-arg)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: value => null()

        contains

        final :: finalizeBase
    end type

    type container(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data
    end type

    contains

    subroutine abc (x)
        class (*), intent(out) :: x
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%value)) then
            deallocate (b%value)
            print *, 'data deallocated'
        end if
    end subroutine
end module

program fArg519a1
use m
    class (*), pointer :: x

    allocate (base(4):: x)

    call abc (x)

    print *, '2nd test'

    allocate (container(4) :: x)

    select type (x)
        class is (container(4))
            allocate (x%data)
        class default
            error stop 1_4
    end select

    call abc(x)

    print *, 'end'
end