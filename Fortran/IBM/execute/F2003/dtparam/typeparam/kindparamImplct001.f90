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
!*  DATE                       : 01/09/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in
!                               declaration-type-spec: implict stmt.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        integer(k) :: id(2) = -1
    end type
end module

program kindparamImplct001
use m
    implicit type(base(k=8)) (a-b), type(base(4)) (d), type(base(k=2)) (f-h)

    type(base(1)) b1

    dimension b2(10)

    a1%id = 2_8**35+(/10, 20/)

    b2(1)%id = 2_8**35*(/2, 3/)

    df%id = (/100, 200/)
    g3%id = 1

    !! verify
    if (any (dk%id /= (/-1, -1/))) error stop 1_4

    if (range(b1%id(2)) /= range (1_1)) error stop 2_4

    if (any((a1%id - 15)/(2**30) /= (/31, 32/))) error stop 3_4

    if (any(b2(1)%id/2**30 /= (/64, 96/))) error stop 4_4

    if (any (b2(3)%id /= -1)) error stop 5_4

    if (any (df%id /= (/100, 200/))) error stop 6_4

    if (range(g2%id) /= range(1_2)) error stop 7_4
    if (any (g3%id /= 1)) error stop 8_4
end
