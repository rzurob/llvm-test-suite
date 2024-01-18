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
!*  PROGRAMMER                 : Gaby Baghdadi
!*  DATE                       : 08/28/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 340417)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
  type :: base(n)
    integer, len :: n
    character(n) :: c
  end type
end module

use m
class(*), pointer :: ptr1
class(base(20)), pointer :: ptr2
allocate(base(*)::ptr1)       ! <-- illegal
allocate(base(*)::ptr2)       ! <-- illegal
end
