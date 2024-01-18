! TS29113 C-Interop Optional arguments
! Check that the order of value and optional doesn't matter.
! (always illegal)
subroutine foo(a, b, c, d) bind(c)
  use iso_c_binding
  integer(C_INT), value, optional :: a ! Error
  integer(C_INT), optional, value :: b ! Error
  integer(C_INT) :: c
  optional :: c
  value :: c                           ! Error
  integer(C_INT) :: d
  value :: d
  optional :: d                        ! Error
end subroutine
