!In module procedure interface body, the type parameter, bounds of one argument 
!  may depend on another dummy argument. That dependency should be available in 
!  the corresponding separate module subprogram.

module mmm
  type base(k, l)
    integer, kind :: k
    integer, len :: l 
    integer :: i = 4
  end type
end module

module nnn
  use mmm
  interface
!    module type(base(4, :)) function foo(arg1, arg2)
    module function foo(arg1, arg2)
      integer :: yyy = 5
      type(base(4, :)) :: foo
      allocatable :: foo
      integer, intent(in) :: arg1 
      integer, intent(in) :: arg2(arg1-4:arg1+2+kind(arg1))
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
    foo = base(4, arg1)()
    print*, foo%i
    print*, foo%l
    foo%i = 5
    print*, foo%i
    print*, foo%l
    print*, yyy
  end procedure
end submodule

program mod_subprog
  use mmm
  use nnn
  integer, allocatable ::  i(:)
  integer j
  type(base(4, :)), allocatable :: b1

  j = 10
  allocate(i(j))
  b1 = foo(j, i)
  print*, b1
end
 

