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
!*  DATE                       : 08/16/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 340462)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n)
        integer, len :: n

        real(8) data(n)
    end type

    type container (n, m)
        integer, len :: n, m

        type(base(n)) data(m)
    end type
end module

use m
    integer n1, n2
    type(container(:,:)), allocatable :: co1

    n1 = 100
    n2 = 10

    allocate (container(n1, n2) :: co1)

    co1%data(1) = base(n1) (-1.0)

    write (*, '(100f5.1)') co1%data(1)
end
