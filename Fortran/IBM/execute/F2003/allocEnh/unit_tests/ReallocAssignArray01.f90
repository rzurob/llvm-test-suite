! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 5, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment
!*                               with array components, using a user
!*                               procedure for subscripting.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

integer :: count = 0
type dt
  integer, allocatable :: i(:)
end type
type(dt) :: x(10), y(5)
allocate(y(foo())%i(2:3))
x(foo())%i = y(foo())%i
if (.not. allocated(x(foo())%i)) stop 1
if (any(shape(x(foo())%i) .ne. (/2/))) stop 2
if (lbound(x(foo())%i,1) /= 2) stop 3
if (ubound(x(foo())%i,1) /= 3) stop 4
if (count /= 7) stop 5
contains
  integer function foo()
    foo = 2
    count = count + 1
  end function
end
