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
!*  DATE                       : 11/22/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Statement: An extended type includes all of the
!                               type parameters of its parent type.
!                               Case: use the type parameters of base type in
!                               the extended type of the 3rd generation.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1, l1)
        integer, kind :: k1
        integer, len :: l1

        integer(k1) :: id = -1
        character(l1) :: name = 'nameless'
    end type

    type, extends(base) :: child
        logical(k1) :: flag
    end type

    integer, parameter :: single = 4
    integer, parameter :: double = 8
    integer, parameter :: nameLen = 6

    class(base(single, nameLen)), pointer :: b1_m => null()
end module


module m1
use m
    type, extends(child) :: gen3
        real(k1*2) :: data(l1) = 1.0_8
    end type
end module


program dtparamExtends003
use m1
    type (gen3(double, nameLen+5)) :: g1
    type (gen3(single, nameLen)), target :: g2(10)

    !! verify the derived type parameter values and component values
    if ((g1%k1 /= double) .or. (g1%l1 /= nameLen+5)) error stop 1_4

    b1_m => g2(3)

    if ((g2%k1 /= single) .or. (g2%l1 /= nameLen)) error stop 2_4
    if ((b1_m%k1 /= single) .or. (b1_m%l1 /= nameLen)) error stop 3_4

    if ((b1_m%id /= -1) .or. (b1_m%name /= 'namele')) error stop 4_4

    if ((g1%id /= -1) .or. (g1%name /= 'nameless')) error stop 5_4

    if (any(g1%data /= (/(1.0_8, i=1,nameLen+5)/))) error stop 6_4 !<--on purpose

    if ((any(g2%id /= -1)) .or. (any(g2%name /= 'namele'))) error stop 7_4

    if (any(g2(3)%data /= (/(1.0_8, i=1,nameLen)/))) error stop 8_4 !<--on purpose
end
