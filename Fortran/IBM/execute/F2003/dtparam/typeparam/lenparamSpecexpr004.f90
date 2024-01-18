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
!*  DATE                       : 01/04/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: length type parameters for procedure
!                               pointer components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n = 10

        real data(n)
    end type

    type A (k, l)
        integer, len :: l
        integer, kind :: k = 4

        integer(k) :: id
        procedure(type(base(l))), pointer, nopass :: p1 => null()
        procedure(type(base(k+l))), pointer, nopass :: p2 => null()
    end type
end module

program lenparamSpecexpr004
use m
    type (A(4, 10)) a1
    type (A(8, 12)) a2(10)

    type(base(10)), external :: genBase10
    type(base(20)), external :: genBase20

    type(base(10)) b1
    type(base(20)) b2

    logical(4) precision_r4

    a1%id = 1

    a2%id = (/(i*2_8**32, i = 1, 10)/)

    a1%p1 => genBase10
    a2(10)%p2 => genBase20

    b1 = a1%p1()
    b2 = a2(10)%p2()

    !! verify the results
    if ((a1%id /=1) .or. any(a2%id/2**29 /= (/(8*i, i=1, 10)/))) error stop 1_4

    do i = 1, 10
        if (.not. precision_r4(b1%data(i), i*1.0e0)) error stop 2_4
    end do

    do i = 1, 20
        if (.not. precision_r4(b2%data(i), i*1.0e0)) error stop 3_4
    end do
end

function genBase10()
use m
    type(base(10)) genBase10

    genBase10%data = (/(i*1.0e0, i=1,10)/)
end function


function genBase20()
use m
    type(base(20)) genBase20

    genBase20%data = (/(i*1.0e0, i=1,20)/)
end function
