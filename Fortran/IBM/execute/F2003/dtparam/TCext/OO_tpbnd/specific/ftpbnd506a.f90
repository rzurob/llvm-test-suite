! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd506a.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2005
!*
!*  DESCRIPTION                : specific type bound (specific type bound in a
!                               where construct)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), pointer :: data => null()

        contains

        procedure :: sameType => containSameType
        final :: finalizeBase
    end type

    interface assignment (=)
        module procedure assgnInt2Base
    end interface

    contains

    elemental logical function containSameType (b1, x)
        class (base(4,*)), intent(in) :: b1
        class (*), intent(in) :: x

        if (associated(b1%data)) then
            containSameType = same_type_as (b1%data, x)
        else
            containSameType = .false.
        end if
    end function

    elemental subroutine assgnInt2Base (b, data)
        class (base(4,*)), intent(out) :: b
        integer, intent(in) :: data

        allocate (b%data, source=data)
    end subroutine

    elemental subroutine finalizeBase (b)
        type (base(4,*)), intent(inout) :: b

        if (associated(b%data)) deallocate (b%data)
    end subroutine
end module

program ftpbnd506a
use m
    class (base(4,:)), allocatable :: b1(:)

    logical(4) precision_r8

    allocate (base(4,20) :: b1(100))

    do i = 1, 100, 3
        allocate (b1(i)%data, source=(1.0_8*i))

        if (i < 100) allocate (character(20) :: b1(i+1)%data)
    end do

    where (.not. b1%sameType(1.0_8))
        b1 = 1
    end where

    !! verify the results
    if (.not. all (b1(::3)%sameType (1.0_8))) error stop 1_4

    if (.not. all (b1(2::3)%sameType (1))) error stop 2_4
    if (.not. all (b1(3::3)%sameType (1))) error stop 3_4

    do i = 1, 100, 3
        select type (x1 => b1(i)%data)
            type is (real(8))
                if (.not. precision_r8 (x1, 1.0_8*i)) error stop 4_4
            class default
                error stop 6_4
        end select

        if (i < 100) then
            select type (x2 => b1(i+1)%data)
                type is (integer)
                    if (x2 /= 1) error stop 7_4
                class default
                    error stop 8_4
            end select

            select type (x3 => b1(i+2)%data)
                type is (integer)
                    if (x3 /= 1) error stop 9_4
                class default
                    error stop 10_4
            end select
        end if
    end do
end
