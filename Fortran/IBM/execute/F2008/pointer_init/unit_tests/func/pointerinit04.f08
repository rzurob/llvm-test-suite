subroutine test
  implicit none
  interface
    integer function foo(a)
      integer, value :: a
    end function
    integer function myfoo(a)
      integer, value :: a
    end function
  end interface
  type dt
    integer :: i = 3
    procedure(foo), pointer, nopass :: p => myfoo
  end type

  type(dt), allocatable :: a
  type(dt) c
  type(dt), save :: d
  integer i

  allocate(a)
  if (.not. associated(a%p)) error stop 1
  if (.not. associated(a%p, myfoo)) error stop 2
  i = a%p(5)
  if (i /= 8) error stop 3

  block
    type(dt) b
    i = 0
    if (b%i /= 3) error stop 999
    if (.not. associated(b%p)) error stop 4
    if (.not. associated(b%p, myfoo)) error stop 5
    i = b%p(5)
    if (i /= 8) error stop 6
  end block

  i = 0
  if (.not. associated(c%p)) error stop 7
  if (.not. associated(c%p, myfoo)) error stop 8
  i = c%p(5)
  if (i /= 8) error stop 9

  i = 0
  if (.not. associated(d%p)) error stop 10
  if (.not. associated(d%p, myfoo)) error stop 11
  i = d%p(5)
  if (i /= 8) error stop 12

end subroutine

integer function myfoo(a)
  implicit none
  integer, value :: a
  myfoo = a + 3
end function

program main
  call test
end program
