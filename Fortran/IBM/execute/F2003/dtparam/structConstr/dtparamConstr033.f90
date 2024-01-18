!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of the structure constructor in a
!                               strutcure of unlimited poly allocatable
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type container
        class(*), allocatable :: data
    end type

    type base(k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type(container) :: co1

    logical(4), external :: precision_r4
end module

program dtparamConstr033
use m
    co1 = container(base(8,10)(sqrt(1.0*(/(i, i=1,10)/))))

    !! verify
    select type (x => co1%data)
        type is (base(8,*))
            if (x%n /= 10) error stop 1_4

            do i = 1, 10
                if (.not. precision_r4(real(x%data(i)), sqrt(1.0*i))) &
                    error stop 2_4
            end do

        class default
            error stop 10_4
    end select
end
