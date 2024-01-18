!In module procedure interface body, the type parameter, bounds of one argument
!  may depend on another dummy argument. That dependency should be available in
!  the corresponding separate module subprogram.

module mmm
  type base
    integer :: i = 4
  end type
end module

module nnn
  use mmm
  interface
    module type(base) function foo(arg1, arg2)
      integer :: yyy = 5
      integer, intent(in) :: arg1
      integer, intent(in), optional :: arg2(arg1)
    end
  end interface
end module

submodule (nnn) submod
contains
  module procedure foo
    implicit none
    print*, arg1
    arg1 = 4

    if (present(arg2)) then
      print*, ubound(arg2) !! should be the value of arg1
    end if

    foo = base()
    print*, foo
    foo%i = 5
    print*, foo
    print*, yyy
  end procedure
end submodule

program mod_subprog
  use mmm
  use nnn
  integer, allocatable ::  i(:)
  integer j
  type(base) :: b1

  j = 10
  allocate(i(j))
  b1 = foo(j, i)
  print*, b1
end

