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
!*  DATE                       : 01/10/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in
!                               declaration-type-spec: entities declaration
!                               statement; use automatic variables (length
!                               type-parameter not init-expr)
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

        contains

        generic :: sum => sum4, sum8
        procedure :: sum4 => sumData4
        procedure :: sum8 => sumData8
    end type

    contains

    real(4) function sumData4 (b)
        class (base(4, *)), intent(in) :: b

        sumData4 = sum (b%data)
    end function

    real(8) function sumData8 (b)
        class (base(k=8, n=*)), intent(in) :: b

        sumData8 = sum (b%data)
    end function
end module

program kindparamDTSpec001
    real(4) r1(10000), rsum
    real(8) d1(20000), dsum

    logical(4), external :: precision_r4, precision_r8

    call random_number(r1)
    call random_number(d1)

    call sum1(r1(4000), 4000, rsum)
    call sum2(d1(5000), 5000, dsum)

    !! verify
    if (.not. precision_r4(rsum, sum (r1(4000:7999)))) error stop 1_4
    if (.not. precision_r8(dsum, sum (d1(5000:9999)))) error stop 2_4
end

subroutine sum1 (r1, n, sum)
use m
    real(4), intent(in) :: r1(n)
    integer, intent(in) :: n
    real(4), intent(out) :: sum

    type (base(4,n)) b1

    b1%data = r1

    sum = b1%sum()
end subroutine

subroutine sum2 (d1, n, sum)
use m
    real(8), intent(in) ::d1(n)
    integer, intent(in) :: n
    real(8), intent(out) :: sum

    type (base(k=8, n=n)) b1

    b1%data = d1

    sum = b1%sum()
end subroutine
