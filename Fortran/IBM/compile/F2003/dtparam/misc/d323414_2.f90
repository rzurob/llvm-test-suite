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
!*  DATE                       : 09/13/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 323414.2)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType
        real(8), allocatable :: data(:)
    end type

    type base
        type(dataType), allocatable :: data(:)
    end type

end module

use m
    type(base), allocatable :: b1(:)

    i = 10
    j = 10
    allocate (b1(10))

    allocate (b1(10)%data(10))

    print *, allocated (b1(i)%data(j)) !<-- illegal
end

