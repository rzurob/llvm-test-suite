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
!*  TEST CASE TITLE            : MoveAllocRank2Array13
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

type dt(x)
  integer, len :: x
  integer :: i(x)
end type
type(dt(2)), allocatable, target :: foo(:,:)
call sub(foo)
contains
  subroutine sub(a)
    type(dt(*)), allocatable, target :: a(:,:)
    type(dt(:)), allocatable, target :: b(:,:)
    type(dt(:)), pointer :: p(:,:)
    allocate(a(10,10))
    p => a
    call move_alloc(a,b)
    if (allocated(a)) stop 1
    if (.not.allocated(b)) stop 2
    if (.not.associated(p,b)) stop 3
  end subroutine
end
