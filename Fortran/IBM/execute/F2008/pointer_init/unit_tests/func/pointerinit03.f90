module m
  integer i
end module

program main
  use m, only: i
  implicit none
  interface
    subroutine sub(a)
      integer, value :: a
    end subroutine
    subroutine mysub(a)
      integer, value :: a
    end subroutine
  end interface
  procedure(sub), pointer :: p => mysub

  if (.not. associated(p)) error stop 1
  if (.not. associated(p, mysub)) error stop 2
  call p(5)
  if (i /= 5) error stop 3
end

subroutine mysub(a)
  use m, only: i
  implicit none
  integer, value :: a
  i = a
end subroutine
