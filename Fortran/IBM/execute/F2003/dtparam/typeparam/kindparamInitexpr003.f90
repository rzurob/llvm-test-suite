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
!*  DATE                       : 12/24/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case:  the kind type parameters in default init
!                               for components: array component and array
!                               constructors.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n, start)
        integer, kind :: n, start

        integer :: ids(n) = (/(start+i, i=0, n-1)/)
    end type
end module

program kindparamInitexpr003
use m
    type (base(100, 0)) b1

    type (base(20, 100)), allocatable :: b2(:,:)

    class (base(-2, 10000)), pointer :: b3

    allocate (b2(10, 10), b3)

    !! verify the default values
    if (any(b1%ids /= (/(i, i=0, 99)/))) error stop 1_4

    if (any (b2(2, 10)%ids /= (/(i, i=100, 119)/))) error stop 2_4

    if (any (b2(2, 10)%ids /= b2(1,1)%ids)) error stop 3_4

    if (size(b3%ids) /= 0) error stop 4_4
end
