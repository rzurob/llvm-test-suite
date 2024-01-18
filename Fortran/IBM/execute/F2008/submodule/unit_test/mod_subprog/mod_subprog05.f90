! Test OPTIONAL, DIMENSION statements are written/read to/from the module

module mmm
  type base
    integer :: i = 4
  end type
end module

module nnn
  use mmm
  interface
    module type(base) function foo(arg1, arg2, arg3)
      integer :: yyy = 5
      integer, intent(in) :: arg1 
      integer, intent(in) :: arg2
      integer arg3
      optional :: arg3
      dimension :: arg2(arg1-4:arg1+2+kind(arg1))
    end
  end interface
end module

submodule (nnn) submod
contains
  module procedure foo
    integer :: yyy = 6
    print*, arg1
    print*, lbound(arg2)
    print*, ubound(arg2)
    foo = base()
    print*, foo
    foo%i = 5
    print*, foo
    print*, yyy
    if (present(arg3)) then
      print*, arg3
    end if
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
 

