! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/generic/unit_tests/diag/genop18d.f
! opt variations: -qnok -qnol

! Generic interface block conflicts with generic binding.
! (With the generic binding appearing first)

module m
  type dt(k1,n1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: n1
  contains
    procedure :: foo
    generic :: operator(+) => foo
  end type
contains
  integer function foo(a,i)
    class(dt(4,*)), intent(in) :: a
    integer, intent(in) :: i
    print *, "foo"
    foo = i
  end function
end module

use m
interface operator(+)
  integer function bar(a,i)
    use m, only: dt
    class(dt(4,*)), intent(in) :: a
    integer, intent(in) :: i
  end function
end interface
type(dt(4,20)) a
integer ii
ii = a + 4
end

integer function bar(a,i)
  use m, only: dt
  class(dt(4,*)), intent(in) :: a
  integer, intent(in) :: i
  print *, "bar"
  bar = i+1
end function
