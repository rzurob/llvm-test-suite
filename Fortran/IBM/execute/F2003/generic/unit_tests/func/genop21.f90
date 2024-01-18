! Generic operator bindings: Elemental vs Non-elemental in generic resolution.

module m
  type dt
    integer i
  contains
    generic :: operator(-) => subt
    procedure :: subt  ! non-elemental proc
  end type

  interface operator(-)
    module procedure subte ! elemental proc
  end interface
contains
  elemental integer function subte(a, b)
    class(dt), intent(in) :: a
    integer, intent(in) :: b
    subte = -1
  end function

  function subt(a, b)
    class(dt), intent(in) :: a
    integer, intent(in) :: b(3)
    integer subt(3)
    subt = -100
  end function
end module

use m
class(dt), allocatable :: x
integer res(3)
integer :: arg2(3) = 3
allocate(x, source=dt(3))

! Case 1: Should resolve to elemental routine subte
res = x - 3
if (any(res /= -1)) then
  print *, res
  error stop 1_4
endif

! Case 2: Should resolve to non-elemental routine subt
!         Both subt and subte match, but non-elemental
!         has higher precedence.
res = x - arg2
if (any(res /= -100)) then
  print *, res
  error stop 2_4
endif
end
