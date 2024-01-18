module m1 
  integer, parameter :: k = 8
  integer, parameter :: k2 = 4 
  integer :: l = 11

  type dt 
    integer :: a = 4
  end type
end module

module o1
 integer xx
end module

module m3
integer :: l = 15
type dt 
end type
end

module m4
integer :: j = 7
end module

module m2 
  interface
    module subroutine sub1(arg1, arg2, arg3, arg4, arg5)
      use m1
      use o1
      integer(kind=k) :: arg1
      type(dt) arg2
      integer :: arg3(k+k2)
      integer :: arg4(l)
      interface
        integer function arg5(hh)
          use m4
          real :: hh(j)
        end
      end interface
    end

    module subroutine sub2(arg6)
      use m1, n => l
      integer :: arg6(n)
    end

    type(dt) module function foo(arg7)
      use m1
      pointer foo
      type(dt) :: arg7
    end
 
  end interface

end module

submodule (m2) s1 
contains
  module procedure sub1
    print*, kind(arg1)     ! Q1
    print*, arg2%a         ! Q2
    print*, ubound(arg3)   ! Q3
    print*, ubound(arg4)   ! Q4 
  end

  module procedure sub2
    use m1
    integer :: n = 9
    print*, ubound(arg6)   ! Q5
  end

  module procedure foo
    print*, foo%a
  end
end submodule

program mod_subprog09
  use m2
  integer :: aa(5)
  call sub2(aa)
end
