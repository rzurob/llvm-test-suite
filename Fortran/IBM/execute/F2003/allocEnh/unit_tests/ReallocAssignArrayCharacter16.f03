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

@process xlf2003(noautorealloc)
character(:), allocatable :: a(:)
character(5) :: b(5)
allocate(character(3) :: a(4))
b = (/(char(i),i=1,5)/)
a = b
if (.not. allocated(a)) error stop 1
if (any(shape(a) /= shape(b))) error stop 2
if (lbound(a,1) /= 1) error stop 3
if (ubound(a,1) /= 5) error stop 4
if (any(a /= (/(char(i),i=1,5)/))) error stop 5
if (len(a) /= 5) error stop 6
end