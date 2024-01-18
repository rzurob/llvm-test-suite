! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 12, 2006
!*
!*  DESCRIPTION                : Testing reallocation on assignment,
!*                               with a scalar, deferred-length
!*                               character on the left-hand side.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

call sub(12)
contains
  subroutine sub(n)
    integer n
    character(:), allocatable :: a
    character(n) b
    b = 'abcdefghijkl'
    allocate(character(40) :: a)
    a = b
    if (.not.allocated(a)) error stop 1
    if (len(a) /= 12) error stop 2
    if (a /= 'abcdefghijkl') error stop 3
  end subroutine
end
