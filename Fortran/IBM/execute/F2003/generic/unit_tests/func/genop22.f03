! Generic operator bindings:  elemental vs non-elemental in generic resolution
!                             (polymorphic and non-polymorphic tests)

module m
  type base
    integer :: x
  contains
    generic :: operator(*) => mul
    procedure :: mul => mulbase
  end type

  type, extends(base) :: child
    integer :: y
  contains
    procedure :: mul => mulchild
    generic :: operator(*) => mul
  end type
contains
  elemental integer function mulbase ( a, b )
    class(base), intent(in) :: a,b

    mulbase = a%x * b%x
  end function

  elemental integer function mulchild ( a, b )
    class(child), intent(in) :: a
    class(base), intent(in) :: b

    select type (b)
      type is (base)
        mulchild = a%base * b * a%y
      type is (child)
        mulchild = a%base * b%base * a%y * b%y
    end select
  end function
end module

use m

type(base) :: b1, b2(3)
class(base), allocatable :: b3(:), b4(:)

type(child) :: c1(3)
class(child), pointer :: c2(:)

integer :: res(3)

b1 = base(2)
b2 = (/ base(3),  base(4),  base(5) /)
c1 = (/ child(1,2), child(3,4), child(5,6) /)
allocate ( c2(3), source = (/ (child(i, i+1), i = 2,6,2 ) /) )
allocate ( b3(3), source = (/ (child(i, i+1), i = 2,6,2 ) /) )
allocate ( b4(3), source = (/ (base(i), i = 2,6,2 ) /) )

res =  b1 * b2
if (res(1) /= 6 .or. res(2) /= 8 .or. res(3) /= 10) then
  print *, res
  error stop 1_4
endif

res =  b1 * c1
if (res(1) /= 2 .or. res(2) /= 6 .or. res(3) /= 10) then
  print *, res
  error stop 2_4
endif

res = c1 * c2
if (res(1) /= 12 .or. res(2) /= 240 .or. res(3) /= 1260) then
  print *, res
  error stop 3_4
endif

res = c1 * b3
if (res(1) /= 12 .or. res(2) /= 240 .or. res(3) /= 1260) then
  print *, res
  error stop 4_4
endif

res = b3 * c1
if (res(1) /= 12 .or. res(2) /= 240 .or. res(3) /= 1260) then
  print *, res
  error stop 4_4
endif

res = b4 * b3
if (res(1) /= 4 .or. res(2) /= 16 .or. res(3) /= 36) then
  print *, res
  error stop 5_4
endif

res = b3 * b4
if (res(1) /= 12 .or. res(2) /= 80 .or. res(3) /= 252) then
  print *, res
  error stop 6_4
endif
end
