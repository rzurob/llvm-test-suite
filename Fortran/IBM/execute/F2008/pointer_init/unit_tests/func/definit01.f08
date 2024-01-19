! derived type definition with pointer initialization.
! Derived type object is in automatic storage.
subroutine sub
  implicit none
  integer, target, save :: t(3) = [1, 2, 3]
  type dt
    integer :: i = 3
    integer, pointer :: p => t(2)
  end type
  type(dt) d
  if (.not. associated(d%p)) error stop 1
  if (d%p /= t(2)) error stop 2
end subroutine

program main
  call sub
end program
