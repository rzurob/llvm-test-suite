!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use the unlimited poly pointer component
!                               to be associated with a pointer of parameterized
!                               derived type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type (base(4,:)), pointer :: b1
end module

module n
    type container
        class(*), pointer :: data
    end type

    type (container) :: co1
end module

program dtparamConstr035
use m
use n
    allocate (base(4,155) :: b1)

    co1 = container(b1)

    if (.not. associated(co1%data, b1)) error stop 1_4

    call assgnVal

    call verifyVal
end


subroutine assgnVal
use m, only : base
use n
    select type (x => co1%data)
        type is (base(4,*))
            x = base(4,x%n)(log(1.0e-1+(/(i, i=1,x%n)/)))

        class default
            error stop 10_4
    end select
end subroutine


subroutine verifyVal
use m, only : base
use n
    logical(4), external :: precision_r4

    select type (x => co1%data)
        class is (base(4,*))
            if (x%n /= 155) error stop 15_4

            do i = 1, 155
                if (.not. precision_r4(x%data(i), log(1.0e-1+i))) &
                    error stop 16_4
            end do

        class default
            error stop 20_4
    end select
end subroutine
