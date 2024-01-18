! F2008 generic extensions:  Procedure dummy arguments that are
! not known to be function are indistinguishable from each other.
module m
  implicit none
  integer, parameter :: from_s1 = 1
  integer, parameter :: from_f1 = 2
  integer status
contains
  integer function f1()
    status = from_f1
    f1 = 1
  end function

  subroutine s1()
    status = from_s1
  end subroutine
end module

use m
implicit none

interface foo
  subroutine sub1(x)
    procedure() x
  end subroutine

  subroutine sub2(x)
    procedure() x
  end subroutine
end interface

call foo(f1)
call foo(s1)

! This is a diagnostic test case.  If we passed compilation, we should fail.
error stop 1
end

subroutine sub1(x)
  use m
  implicit none
  procedure(s1) x

  print *, 'sub1'
  call x
end subroutine

subroutine sub2(x)
  use m
  implicit none
  procedure(f1) x

  print *, 'sub2'
  print *, x()
end subroutine
