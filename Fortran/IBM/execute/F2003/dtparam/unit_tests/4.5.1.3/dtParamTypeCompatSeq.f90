! 4.5.1.3 Determination of Types
! The two sequence derived type definitions here are considered
! equivalent.  The type parameters appear in the same order, have
! the same names, and attributes.
type dt2(k2)
  integer, kind :: k2
  sequence
  integer(k2) j
end type

type dt(k,l)
  integer, kind :: k
  integer, len :: l
  sequence
  integer(k) i(l)
  type(dt2(k)), allocatable :: d2
end type

type(dt(2,3)) x

interface
  subroutine sub(a)
    type dt2(k2)
      integer, kind :: k2
      sequence
      integer(k2) j
    end type

    type dt(k,l)
      integer, kind :: k
      integer, len :: l
      sequence
      integer(k) i(l)
      type(dt2(k)), allocatable :: d2
    end type
    type(dt(2,3)) a
  end subroutine
end interface

x%i = (/ 1,2,3 /)
allocate(x%d2)
x%d2%j = 4
call sub(x)
end

subroutine sub(a)
  type dt2(k2)
    integer, kind :: k2
    sequence
    integer(k2) j
  end type
  type dt(k,l)
    integer, kind :: k
    integer, len :: l
    sequence
    integer(k) i(l)
    type(dt2(k)), allocatable :: d2
  end type
  type(dt(2,3)) a
  if (kind(a%i) /= 2) then
    print *, kind(a%i)
    error stop 1
  endif

  if (a%i(1) /= 1 .or. a%i(2) /= 2 .or. a%i(3) /= 3) then
    print *, a%i
    error stop 2
  endif

  if (.not. allocated(a%d2)) error stop 3

  if (a%d2%j /= 4) then
    print *, a%d2%j
    error stop 4
  endif
end subroutine
