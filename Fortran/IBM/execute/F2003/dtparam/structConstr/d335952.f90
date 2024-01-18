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
!*  DATE                       : 04/23/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               miscellaneous (defect 335952)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k)
        integer, kind :: k

        integer(k), allocatable :: id
        type(A(k)), pointer :: next
    end type
end module

use m
    type(A(8)), target :: a1, a2

    type(A(8)), pointer :: p1

    a1 = A(8)(2_8**33-1, null())

    a2 = A(8)(2_8**34-1, a1)

    p1 => a2

    if (.not. allocated(p1%id)) error stop 1_4

    if ((p1%id+1)/1024 /= 2**24) error stop 2_4

    if (.not. associated(p1%next, a1)) error stop 3_4

    p1 => p1%next

    if (.not. allocated(p1%id)) error stop 4_4
    if ((p1%id+1)/1024 /= 2**23) error stop 5_4

    if (associated(p1%next)) error stop 6_4
    end
