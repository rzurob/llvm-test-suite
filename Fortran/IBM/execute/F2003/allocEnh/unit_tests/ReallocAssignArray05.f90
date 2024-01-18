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
!*  TEST CASE TITLE            : ReallocAssignArray05
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : June 5, 2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing reallocation on assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

logical(2), allocatable :: a(:)
allocate(a(3))
a = (/.true.,.false.,.true.,.false./)
if (any(shape(a) /= (/4/))) stop 1
if (any(a .neqv. (/.true., .false., .true.,.false./))) stop 2
end
