module m
  type dt
    real :: i = 1.0
  end type
end module

module ooo
  interface
    module type(dt) function foo(arg1, arg2, dummyproc)
      use m
      integer arg2
      intent(in) :: arg2
      real :: ll = 3.0, arg1, mm = 4.0
      integer k
      dimension k(10), arg1(arg2+2)
      interface
        subroutine dummyproc(aa)
          real aa
        end
      end interface
    end function
  end interface
end module

submodule (ooo) submod
contains
  module procedure foo
    real :: ll = 2.0

    print*, foo%i
    print*, ubound(arg1)
    call dummyproc(ll)
  end procedure
end submodule

program mod_subprog06
  use ooo
  use m
  type(dt) :: d1
  real :: r1(10)
  interface
    subroutine proc(aa)
      real aa
    end
  end interface
  procedure(proc), pointer :: ptr

  ptr => proc
  d1 = foo(r1, 10, ptr)
end

subroutine proc(aa)
  real aa

  print*, aa
end
