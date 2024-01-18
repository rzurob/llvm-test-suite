! 4.5.1.3 Determination of Types
! The two sequence derived type definitions here are considered
! equivalent.  The type parameters have different names 
! the same names, and attributes.
type dt2(o2)
  integer, kind :: o2
  sequence
  integer(o2) j
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
