! derived type definition with pointer initialization.
! Derived type object is static
module m
  implicit none
  integer, target, save :: t(3) = [1, 2, 3]
  type dt
    integer :: i = 3
    integer, pointer :: p => t(2)
  end type
  type(dt) d1(2)
end module

subroutine sub
  use m
  implicit none
  type(dt), save :: d2

  if (.not. associated(d1(1)%p)) error stop 1
  if (.not. associated(d1(2)%p)) error stop 2
  if (d1(1)%p /= t(2)) error stop 3
  if (d1(2)%p /= t(2)) error stop 4

  if (.not. associated(d2%p)) error stop 5
  if (d2%p /= t(2)) error stop 6
end subroutine

program main
  call sub
end program
