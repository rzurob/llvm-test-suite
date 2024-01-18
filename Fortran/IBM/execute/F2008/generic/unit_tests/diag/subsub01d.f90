! F2008 generic extensions:  Subroutine dummy argument are
! indistinguishable from each other
module m
  implicit none
  integer, parameter :: from_sub1 = 1
  integer, parameter :: from_sub2 = 2
  integer status

contains
  subroutine s1(x)
    real x
    print *, x
    status = from_sub1
  end subroutine

  subroutine s2(x)
    integer x
    print *, x
    status = from_sub2
  end subroutine
end module

use m
implicit none

interface foo
  subroutine sub1(x)
    import s1
    procedure(s1) x
  end subroutine

  subroutine sub2(x)
    import s2
    procedure(s2) x
  end subroutine
end interface

call foo(s1)

! This is a diagnostic test case.  If we passed compilation, we should fail.
error stop 1
end

subroutine sub1(x)
  use m
  implicit none
  procedure(s1) x

  print *, 'sub1'
  call x(2.0)
  status = from_sub1
end subroutine

subroutine sub2(x)
  use m
  implicit none
  procedure(s2) x

  print *, 'sub2'
  call x(2)
  status = from_sub2
end subroutine
