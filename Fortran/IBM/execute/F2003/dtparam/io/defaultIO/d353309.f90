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
!*  DATE                       : 01/26/2009
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (IO: defect 353309)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type X (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    contains

    subroutine t (x1)
        type(X(4, *)) :: x1(:)

        namelist /nml/ x1

        write (*, nml)
    end subroutine
end module

use m
    type(X(4, 2)) :: x1(2)

    x1(1)%data = [1,2]
    x1(2)%data = [10,20]

    call t (x1)
end

