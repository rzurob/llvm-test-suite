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
!*  TEST CASE TITLE            : MoveAllocRank2Array24
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 04/28/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : Use of MOVE_ALLOC with allocatable
!*                               rank-2 arrays
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t
  integer, allocatable :: x(:)
end type
type(t), target :: a(5), b(10)
integer, pointer :: p(:)
allocate(a(3)%x(5))
p => a(3)%x
call move_alloc(a(3)%x,b(5)%x)
if (allocated(a(3)%x)) stop 1
if (.not.allocated(b(5)%x)) stop 2
if (.not.associated(p,b(5)%x)) stop 3
end
