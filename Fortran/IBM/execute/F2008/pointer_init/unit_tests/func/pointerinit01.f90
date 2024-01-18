! Data pointer initialization
! Multiple initializations in the same file result in multiple
! static initializiation procedures
! The name of the static initializer should be based on the binding
! label / mangled name of the compilation unit.
module m
  integer, target :: tt
  integer, pointer :: qq => tt
end module

program main
  use m
  implicit none
  interface
    subroutine sub() bind(c, name='foo')
    end subroutine
  end interface

  integer, pointer :: pp => tt

  tt = 3

  if (.not. associated(qq)) error stop 1
  if (qq /= tt) error stop 2
  if (.not. associated(pp)) error stop 3
  if (pp /= tt) error stop 4

  call sub
end

subroutine sub() bind(c, name='foo')
  use m, only: ltt => tt
  implicit none
  integer, pointer :: oo => ltt
  if (.not. associated(oo, ltt)) error stop 5
end subroutine

subroutine sub()
  real, target, save :: tt
  real, pointer :: pp => tt
  tt = 3.0
  if (.not. associated(pp, tt)) error stop 6
  print *, pp
end subroutine
