! 4.5.1.3 Determination of Types
! The BIND attribute is not allowed on types with type parameters.
type, bind(c) :: dt2(k2)
  integer, kind :: k2
  integer(k2) j
end type

type, bind(c) :: dt(k,l)
  integer, kind :: k
  integer, len :: l
  integer(k) i(l)
  type(dt2(k)) d2
end type

type(dt(2,3)) x

interface
  subroutine sub(a)
    type, bind(c) :: dt2(k2)
      integer, kind :: k2
      integer(k2) j
    end type

    type, bind(c) :: dt(k,l)
      integer, kind :: k
      integer, len :: l
      integer(k) i(l)
      type(dt2(k)) d2
    end type
    type(dt(2,3)) a
  end subroutine
end interface

x%i = (/ 1,2,3 /)
x%d2%j = 4
call sub(x)
end

subroutine sub(a)
  type, bind(c) :: dt2(k2)
    integer, kind :: k2
    integer(k2) j
  end type

  type, bind(c) :: dt(k,l)
    integer, kind :: k
    integer, len :: l
    integer(k) i(l)
    type(dt2(k)) d2
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

  if (a%d2%j /= 4) then
    print *, a%d2%j
    error stop 4
  endif
end subroutine
