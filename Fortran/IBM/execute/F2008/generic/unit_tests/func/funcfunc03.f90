! F2008 generic extensions:  Dummy arguments that are functions with
! results of different kind are distinguishable.
module m
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer status

  abstract interface
    function real4func(x)
      real(4), value :: x
      real(4) real4func(1)
    end function

    function real8func(x)
      real(4), value :: x
      real(8) real8func(1)
    end function
  end interface
end module

use m
implicit none

interface foo
  subroutine sub1(x)
    import real4func
    procedure(real4func) x
  end subroutine

  subroutine sub2(x)
    import real8func
    procedure(real8func) x
  end subroutine
end interface

procedure(real4func) bar1
procedure(real8func) bar2

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
  real(4), value :: x
  real(4) bar1(1)

  bar1 = [ x ]
end function

function bar2(x)
  implicit none
  real(4), value :: x
  real(8) bar2(1)

  bar2 = [ real(8) :: x ]
end function

subroutine sub1(x)
  use m
  implicit none
  procedure(real4func) x

  print *, 'sub1'
  print *, x(2.0)
  status = from_sub1
end subroutine

subroutine sub2(x)
  use m
  implicit none
  procedure(real8func) x

  print *, 'sub2'
  print *, x(2.0)
  status = from_sub2
end subroutine
