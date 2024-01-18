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
!*  DATE                       : 02/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Procedure pointer with PASS attribute;
!                               derived-type with default type parameter values.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 48

        real(k) :: data(n)
    end type

    type container
        type(base) :: data
        procedure(returnVal), pointer :: val => null()
    end type

    abstract interface
        function returnVal (co) result (ret)
        import
            class(container), intent(in) :: co
            type (base) ret
        end function
    end interface
end module

program dtparamDefVal012
use m
    procedure(returnVal) returnData

    class(container), allocatable :: b1

    type (base) :: b2

    logical(4), external :: precision_r8

    allocate (b1, source=container(base(exp((/(i*1.0d1, i=1, 48)/))), &
            returnData))

    b2 = b1%val()

    !! verify
    do i = 1, 48
        if (.not. precision_r8(b2%data(i), exp(i*1.0d1))) error stop 1_4
    end do
end

function returnData (co)
use m
    class(container), intent(in) :: co
    type (base) returnData

    returnData = co%data
end function
