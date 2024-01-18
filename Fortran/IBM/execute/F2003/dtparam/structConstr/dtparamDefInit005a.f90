!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/14/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of the named constants for the default
!                               initializations.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    real(8), parameter :: d_const(100) = sqrt((/(i*1.0d0, i=1,100)/))

    type base (k, n)
        integer, kind :: k, n

        real(k) :: data(n) = (/(d_const, i=1,n/100), &
            sqrt((/(i*1.0d0, i=1,mod(n,100))/))/)
    end type

    type(base(4,36)) :: b1 = base(4,36)()
end module

program dtparamDefInit005a
use m
    type(base(8,500)) b2(10)

    logical(4), external :: precision_r4, precision_r8

    b2 = base(8,500)()

    !! verify
    do i = 1, 36
        if (.not. precision_r4(b1%data(i), real(sqrt(i*1.d0), 4))) &
                error stop 1_4
    end do

    do i = 1, 10
        do j = 1, 500
            if (mod(j,100) == 0) then
                if (.not. precision_r8(b2(i)%data(j), sqrt(100.d0))) &
                    error stop 2_4
            else
                if (.not. precision_r8(b2(i)%data(j), sqrt(mod(j,100)*1.0d0))) &
                    error stop 3_4
            end if
        end do
    end do
end

