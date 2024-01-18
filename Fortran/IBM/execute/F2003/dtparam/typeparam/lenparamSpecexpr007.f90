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
!*  DATE                       : 01/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: use of DIMENSION keyword in the
!                               array components' declarations.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n = 10
    end type

    type, extends(base) :: child (m)
        integer(8), len :: m

        integer(8), dimension(n, m) :: values = -1
    end type
end module

program lenparamSpecexpr007
use m
    type (child(5, 2)) c1
    type (child(m=12)), allocatable, dimension(:) :: c2

    allocate (c2(10))

    c1%values = reshape((/(i, i=1, 10)/), (/5,2/))

    c2(1)%values = reshape((/((i*100+j, i=1, 10), j=1,12)/), (/10, 12/))

    c2(9) = c2(1)

    !! verify results
    if (any(c1%values(:,1) /= (/1,2,3,4,5/))) error stop 1_4

    if (any(c2(4)%values /= -1)) error stop 2_4

    do i = 1, 10
        if (any (c2(9)%values(i,:) /= (/(100*i+j, j=1,12)/))) error stop 3_4
    end do
end
