!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Function resultis polymorphic; derived
!                               type with type parameters using default values.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 8

        integer(k) :: id
    end type

    type, extends(base) :: child (n, l)
        integer, len :: n = 100, l=20

        character(l) :: name
        real(k) :: data(n)
    end type

    contains

    class(base) function produceBase (id, name, data)
        integer(8), intent(in) :: id
        character(*), intent(in), optional :: name
        real(8), intent(in), optional :: data(100)

        allocatable :: produceBase

        if (present(name) .and. present(data)) then
            allocate(produceBase, source=child(id, name, data))
        else
            allocate(produceBase, source=base(id))
        end if
    end function
end module

program dtparamDefVal007a
use m
    logical(4), external :: precision_r8

    select type(x => produceBase(2_8**55, 'xzzzz'))
        type is (base)
            if (x%id/2**29/2**21 /= 32) error stop 1_4

        class default
            error stop 2_4
    end select


    select type (x => produceBase(10_8, &
        'xlftest TC for derived type parameter', (/(i*1.0d0, i=1,200)/)))
        type is (child(n=*,l=*))
            if (x%id /= 10) error stop 3_4
            if (x%name /= 'xlftest TC for deriv') error stop 4_4

            do i = 1, 100
                if (.not. precision_r8(x%data(i), i*1.0d0)) error stop 5_4
            end do

        class default
            error stop 6_4
    end select
end
