! F2008 generic extensions:  Dummy arguments that are functions with
! results of different type are distinguishable.
module m
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer status

  abstract interface
    function realfunc(x)
      real x
      real realfunc(1)
    end function

    function intfunc(x)
      real x
      integer intfunc(1)
    end function
  end interface
end module

use m
implicit none

interface foo
  subroutine sub1(x)
    import realfunc
    procedure(realfunc) x
  end subroutine

  subroutine sub2(x)
    import intfunc
    procedure(intfunc) x
  end subroutine
end interface

procedure(realfunc) bar1
procedure(intfunc) bar2

call foo(bar1)
if (status /= from_sub1) then
  print *, status
  error stop 1
endif

call foo(bar2)
if (status /= from_sub2) then
  print *, status
  error stop 2
endif

end

function bar1(x)
  implicit none
  real x
  real bar1(1)

  bar1 = x
end function

function bar2(x)
  implicit none
  real x
  integer bar2(1)

  bar2 = int(x)
end function

subroutine sub1(x)
  use m
  implicit none
  procedure(realfunc) x

  print *, 'sub1'
  print *, x(2.0)
  status = from_sub1
end subroutine

subroutine sub2(x)
  use m
  implicit none
  procedure(intfunc) x

  print *, 'sub2'
  print *, x(2.0)
  status = from_sub2
end subroutine
