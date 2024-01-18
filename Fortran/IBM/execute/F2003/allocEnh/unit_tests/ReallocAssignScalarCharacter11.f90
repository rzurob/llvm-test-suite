!#######################################################################
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
    character(:), allocatable, target :: a
    character(n) b
    character(:), pointer :: p
    b = 'abcdefghijkl'
    allocate(character(12) :: a)
    p => a
    a = b
    if (.not.allocated(a)) stop 1
    if (len(a) /= 12) stop 2
    if (a /= 'abcdefghijkl') stop 3
    if (.not.associated(p,a)) stop 4
  end subroutine
end
