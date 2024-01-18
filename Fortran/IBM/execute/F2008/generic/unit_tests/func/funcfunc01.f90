! F2008 generic extensions:  Dummy arguments that are functions with
! results of different rank are distinguishable.
module m
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer status

  abstract interface
    function rank1func(x)
      real x
      real rank1func(1)
    end function

    function rank2func(x)
      real x
      real rank2func(1, 2)
    end function
  end interface
end module

use m
implicit none

interface foo
  subroutine sub1(x)
    import rank1func
    procedure(rank1func) x
  end subroutine

  subroutine sub2(x)
    import rank2func
    procedure(rank2func) x
  end subroutine
end interface

procedure(rank1func) bar1
procedure(rank2func) bar2

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
  real bar2(1, 2)

  bar2 = x
end function

subroutine sub1(x)
  use m
  implicit none
  procedure(rank1func) x

  print *, 'sub1'
  print *, x(2.0)
  status = from_sub1
end subroutine

subroutine sub2(x)
  use m
  implicit none
  procedure(rank2func) x

  print *, 'sub2'
  print *, x(2.0)
  status = from_sub2
end subroutine
