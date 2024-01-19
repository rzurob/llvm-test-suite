! Generic type-bound assignment
!   - polymorphic and non-polymorphic cases
!   - elemental

module m
  type base
    integer :: x
  contains
    generic :: assignment(=) => assign
    procedure :: assign => assignbase
  end type

  type, extends(base) :: child
    integer :: y
  contains
    procedure :: assign => assignchild
    generic :: assignment(=) => assign
  end type
contains
  elemental subroutine assignbase ( a, b )
    class(base), intent(out) :: a
    class(base), intent(in) :: b

   a%x = b%x * 2
  end subroutine

  elemental subroutine assignchild ( a, b )
    class(child), intent(out) :: a
    class(base), intent(in) :: b

    select type (b)
      type is (base)
        a%base =  b
        a%x = a%x + 1
        a%y = -1
      type is (child)
        a%base = b%base
        a%x = a%x + 1
        a%y = b%y * 10
    end select
  end subroutine
end module

use m

type(base) :: b1, b2(3)
class(base), allocatable :: b3(:), b4(:)

type(child) :: c1(3), c1r(3)
class(child), pointer :: c2(:), c2r(:)

integer :: res(3)

b1 = base(2)
if (b1%x /= 4) then
  print *, b1%x
  error stop 1_4
endif

b2 = (/ base(3),  base(4),  base(5) /)
if (b2(1)%x /= 6 .or. b2(2)%x /= 8 .or. b2(3)%x /= 10) then
  print *, b2(1)%x, b2(2)%x, b2(3)%x
  error stop 2_4
endif

c1 = (/ child(1,2), child(3,4), child(5,6) /)
if (c1(1)%x /= 3 .or. c1(2)%x /= 7 .or. c1(3)%x /= 11) then
  print *, c1(1)%x, c1(2)%x, c1(3)%x
  error stop 3_4
endif
if (c1(1)%y /= 20 .or. c1(2)%y /= 40 .or. c1(3)%y /= 60) then
  print *, c1(1)%y, c1(2)%y, c1(3)%y
  error stop 4_4
endif

allocate ( c2(3), source = (/ (child(i, i+1), i = 2,6,2 ) /) )
if (c2(1)%x /= 2 .or. c2(2)%x /= 4 .or. c2(3)%x /= 6) then
  print *, c2(1)%x, c2(2)%x, c2(3)%x
  error stop 5_4
endif
if (c2(1)%y /= 3 .or. c2(2)%y /= 5 .or. c2(3)%y /= 7) then
  print *, c2(1)%y, c2(2)%y, c2(3)%y
  error stop 6_4
endif

allocate ( b3(3), source = (/ (child(i, i+1), i = 2,6,2 ) /) )
if (b3(1)%x /= 2 .or. b3(2)%x /= 4 .or. b3(3)%x /= 6) then
  print *, b3(1)%x, b3(2)%x, b3(3)%x
  error stop 7_4
endif
select type (b3)
  type is (child)
    if (b3(1)%y /= 3 .or. b3(2)%y /= 5 .or. b3(3)%y /= 7) then
      print *, b3(1)%y, b3(2)%y, b3(3)%y
      error stop 8_4
    endif
  class default
    error stop 9_4
end select

allocate ( b4(3), source = (/ (base(i), i = 2,6,2 ) /) )

b1 = c1(3)
if (b1%x/= 22) then
  print *, b1%x
  error stop 10_4
endif

c1r = c2
if (c1r(1)%x /= 5 .or. c1r(2)%x /= 9 .or. c1r(3)%x /= 13) then
  print *, c1r(1)%x, c1r(2)%x, c1r(3)%x
  error stop 11_4
endif
if (c1r(1)%y /= 30 .or. c1r(2)%y /= 50 .or. c1r(3)%y /= 70) then
  print *, c1r(1)%y, c1r(2)%y, c1r(3)%y
  error stop 12_4
endif

c1r = b3
if (c1r(1)%x /= 5 .or. c1r(2)%x /= 9 .or. c1r(3)%x /= 13) then
  print *, c1r(1)%x, c1r(2)%x, c1r(3)%x
  error stop 13_4
endif
if (c1r(1)%y /= 30 .or. c1r(2)%y /= 50 .or. c1r(3)%y /= 70) then
  print *, c1r(1)%y, c1r(2)%y, c1r(3)%y
  error stop 14_4
endif

b3 = c1
if (b3(1)%x /= 7 .or. b3(2)%x /= 15 .or. b3(3)%x /= 23) then
  print *, b3(1)%x, b3(2)%x, b3(3)%x
  error stop 15_4
endif
select type (b3)
  type is (child)
    if (b3(1)%y /= 200 .or. b3(2)%y /= 400 .or. b3(3)%y /= 600) then
      print *, b3(1)%y, b3(2)%y, b3(3)%y
      error stop 16_4
    endif
  class default
    error stop 17_4
end select

b4 = b3
if (b4(1)%x /= 14 .or. b4(2)%x /= 30 .or. b4(3)%x /= 46) then
  print *, b4(1)%x, b4(2)%x, b4(3)%x
  error stop 18_4
endif

b3 = b4
if (b3(1)%x /= 29 .or. b3(2)%x /= 61 .or. b3(3)%x /= 93) then
  print *, b3(1)%x, b3(2)%x, b3(3)%x
  error stop 19_4
endif
select type (b3)
  type is (child)
    if (b3(1)%y /= -1 .or. b3(2)%y /= -1 .or. b3(3)%y /= -1) then
      print *, b3(1)%y, b3(2)%y, b3(3)%y
      error stop 20_4
    endif
  class default
    error stop 21_4
end select

end
