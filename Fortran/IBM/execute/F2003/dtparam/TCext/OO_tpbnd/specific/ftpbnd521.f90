! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd521.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2005
!*
!*  DESCRIPTION                : specific type bound (elemental functions and
!                               subroutines)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind     :: k1
        integer, len      :: n1
        real(k1), pointer :: r1(:) => null()

        contains

        procedure :: copy => deepCopy
        final :: finalizeBase
    end type

    contains

    elemental subroutine finalizeBase (b)
        type(base(*,8)), intent(inout) :: b

        if (associated (b%r1)) deallocate (b%r1)
    end subroutine

    elemental function deepCopy (b) result(retVal)
        class (base(*,8)), intent(in) :: b

        type (base(20,8)) retVal

        if (associated (b%r1)) then
            allocate (retVal%r1(size(b%r1)), source=b%r1)
        end if
    end function
end module

program ftpbnd521
use m
    class (base(:,8)), allocatable :: b1(:)

    allocate (base(20,8) :: b1(10))

    associate (x => b1)
        do i = 1, 10
            allocate (x(i)%r1(i), source=(/(j*1.0_8, j =1,i)/))
        end do
    end associate

    associate (x => b1%copy())
        do i = 1, 10
            write (*, '(10f8.2)') x(i)%r1
        end do
    end associate
end
