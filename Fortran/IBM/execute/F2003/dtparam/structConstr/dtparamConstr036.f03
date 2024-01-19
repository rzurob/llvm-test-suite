! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: The data target is a disassociated pointer
!                               of parameterized derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(k/2) :: ids(n)
    end type
end module

module n
    type container
        class(*), pointer :: data
    end type
end module

program dtparamConstr036
use m
use n
    type(container) :: co1

    class(base(8,:)), pointer :: b1

    logical(4), external :: precision_r8

    allocate (b1, source=base(8,67)((/(i, i=1,67)/), ids=-1_8))

    co1 = container(b1)

    if (.not. associated(co1%data, b1)) error stop 1_4

    select type (x => co1%data)
        class is (base(8,*))
            if (any(x%ids /= -1)) error stop 2_4

            do i = 1, 67
                if (.not. precision_r8(x%data(i), real(i, 8))) error stop 3_4
            end do

        class default
            error stop 4_4
    end select

    deallocate (b1)

    co1 = container(b1)

    if (associated(co1%data)) error stop 6_4
end
