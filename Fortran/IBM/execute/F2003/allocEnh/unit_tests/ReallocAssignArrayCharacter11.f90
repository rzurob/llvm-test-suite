! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 9, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with an array of characters on the
!*                               left-hand side of the assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

character(5) :: x(5)
call sub(x)
contains
  subroutine sub(b)
    character(5), allocatable :: a(:)
    character(*) :: b(:)
    allocate(a(2:6))
    b = (/(repeat(char(i),5),i=69,73)/)
    a = b
    if (.not. allocated(a)) error stop 1
    if (any(shape(a) /= shape(b))) error stop 2
    if (lbound(a,1) /= 2) error stop 3
    if (ubound(a,1) /= 6) error stop 4
    if (any(a /= (/(repeat(char(i),5),i=69,73)/))) error stop 5
  end subroutine
end
