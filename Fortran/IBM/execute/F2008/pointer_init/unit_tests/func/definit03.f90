! derived type definition with pointer initialization.
! Derived type definition is use-associated
!  - Pointer init target is use-associated and renamed
!  - Pointer init target is implicitly imported despite use only
module m
  implicit none
  integer, target, save :: t(3) = [1, 2, 3]
  type dt
    integer :: i = 3
    integer, pointer :: p => t(2)
  end type
end module

subroutine sub
  use m, orig_t => t
  implicit none
  integer, target, save :: t(3) = [4, 5, 6]
  type(dt) d
  if (.not. associated(d%p)) error stop 1
  if (d%p /= orig_t(2)) error stop 2
end subroutine

subroutine sub2
  use m, only: dt
  implicit none
  integer, target, save :: t(3) = [4, 5, 6]
  type(dt) d
  if (.not. associated(d%p)) error stop 3
  if (d%p /= 2) error stop 4
end subroutine

program main
  call sub
  call sub2
end program
