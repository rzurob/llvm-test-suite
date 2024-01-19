! 12.3.2.5 Implicit interface specification
! The interface of function foo is implicit.  The function result
! is specified using an explicit type specification of the function name
! There are no constraints in 12.3.2.5, so this test case just tests
! the sanity of the compiler.
type dt(k,l)
  integer, kind :: k
  integer, len :: l
  sequence
  integer(k) :: i(2:l)
end type

type(dt(2,4)) foo
type(dt(2,:)), allocatable :: x
allocate(x, source=foo( dt(2,4)(i=(/1,2,3/)) ))
if (x%i(2) /= 6 .or. x%i(3) /= 7 .or. x%i(4) /= 8) then
  print *, x%i(2:4)
  error stop 1
endif
end

function foo(a) result(res)
 type dt(k,l)
   integer, kind :: k
   integer, len :: l
   sequence
   integer(k) :: i(2:l)
 end type
 type(dt(2,4)), intent(in) :: a
 type(dt(2,4)) res

 res%i = a%i + 5
end function
