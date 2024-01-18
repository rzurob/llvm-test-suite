! F2008 generic extensions:  Dummy arguments that are functions with
! results of the same TKR are indistinguishable.
module m
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer status

  abstract interface
    function rank1func(x)
      real x
      real rank1func(1)
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
    import rank1func
    procedure(rank1func) x
  end subroutine
end interface

procedure(rank1func) bar1

call foo(bar1)
! This is a diagnostic test case.  If it compiles, we failed
error stop 1
end

function bar1(x)
  implicit none
  real x
  real bar1(1)

  bar1 = x
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
  procedure(rank1func) x

  print *, 'sub2'
  print *, x(2.0)
  status = from_sub2
end subroutine
