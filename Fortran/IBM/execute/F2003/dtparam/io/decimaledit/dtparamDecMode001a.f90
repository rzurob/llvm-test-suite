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
!*  DATE                       : 06/27/2008
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               derived type objects with type parameters
!                               appears as the list item in write statement in
!                               a decimal edit mode of comma.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        integer(k) :: id
        real(2*k) :: val(n)
    end type
end module

program dtparamDecMode001
use m
    type (base(4,:)), allocatable :: b1(:)
    integer :: ilen = 15

    allocate (base(4, ilen) :: b1(10))

    do i = 1, 10
        b1(i)%id = i
        do j = 1, ilen
            b1(i)%val(j) = i*100+j
        end do
    end do

    write (*, *, decimal='Comma') b1(1)

    write (*, '(dc, i8, 15d25.10)') b1
end
