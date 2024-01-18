program main
  implicit none
  interface
    integer function foo(a)
      integer, value :: a
    end function
    integer function myfoo(a)
      integer, value :: a
    end function
  end interface
  procedure(foo), pointer :: p => myfoo
  integer i

  if (.not. associated(p)) error stop 1
  if (.not. associated(p, myfoo)) error stop 2
  i = p(5)
  if (i /= 8) error stop 3
end

integer function myfoo(a)
  implicit none
  integer, value :: a
  myfoo = a + 3
end function
