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
!*  DATE                       : 02/03/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: The function return results put into
!                               select type; use the deferred length in the
!                               return result.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        complex :: cx(n)

        contains

        procedure :: add => addBase
    end type

    contains

    function addBase (b1, b2)
        class(base(*)), intent(in) :: b1, b2

        class(base(:)), pointer :: addBase

        complex temps(max(b1%n, b2%n))

        if (b1%n >= b2%n) then
            temps = b1%cx(:b2%n) + b2%cx

            temps(b2%n+1:) = b1%cx(b2%n+1:)
        else
            temps = b2%cx(:b1%n) + b1%cx

            temps(b1%n+1:) = b2%cx(b1%n+1:)
        end if

        allocate (base(max(b1%n, b2%n)) :: addBase)

        addBase%cx = temps
    end function
end module

program dtSpec008a1
use m
    class (base(:)), pointer :: b1
    type (base(10)) b2

    logical(4), external :: precision_x8

    allocate (base(20) :: b1)

    b1%cx = (/((i*1.0, i*2.0), i=1, 20)/)
    b2%cx = (/((i*1.0e1, i*2.0e1), i=1,10)/)

    select type (x => b1%add(b2))
        type is (base(*))
            do i = 1, 10
                if (.not. precision_x8(x%cx(i), (i*1.1e1, i*2.2e1))) &
                    error stop 2_4
            end do

            do i = 11, 20
                if (.not. precision_x8(x%cx(i), (i*1.0, i*2.0))) error stop 3_4
            end do
        class default
            error stop 1_4
    end select

    select type (x => b2%add(b1))
        class is (base(*))
            do i = 1, 10
                if (.not. precision_x8(x%cx(i), (i*1.1e1, i*2.2e1))) &
                    error stop 6_4
            end do

            do i = 11, 20
                if (.not. precision_x8(x%cx(i), (i*1.0, i*2.0))) error stop 7_4
            end do

        class default
            error stop 5_4
    end select
end
