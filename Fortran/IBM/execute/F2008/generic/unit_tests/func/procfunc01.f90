! F2008 generic extensions:  A procedure dummy argument that is
! not known to be a function is distinguishable from a function dummy
! argument if the function dummy argument returns an array.
module m
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer status

  abstract interface
    function rank1func(x)
      real x
      real rank1func(3)
    end function
  end interface
end module

use m
implicit none

interface foo
  subroutine sub1(x)
    procedure() x
  end subroutine

  subroutine sub2(x)
    import rank1func
    procedure(rank1func) x
  end subroutine
end interface

external unknown_proc
procedure(rank1func) bar

call foo(unknown_proc)
if (status /= from_sub1) then
  print *, status
  error stop 1
endif

call foo(bar)
if (status /= from_sub2) then
  print *, status
  error stop 2
endif

end

function unknown_proc(x)
  implicit none
  real x
  real unknown_proc

  unknown_proc = x
end function

function bar(x)
  implicit none
  real x
  real bar(3)

  bar = x
end function

subroutine sub1(x)
  use m
  implicit none
  interface
    real function x(a)
      real y
    end function
  end interface

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
