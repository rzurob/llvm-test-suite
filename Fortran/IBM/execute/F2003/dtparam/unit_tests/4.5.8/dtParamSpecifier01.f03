subroutine sub(x)

type :: dt(k,l)
  integer, kind :: k = 0
  integer, len :: l
  sequence
  integer(k) i(l)
end type

type(dt(k=2,l=*)) x
integer i
do i = 1, ubound(x%i, 1)
  x%i(i) = i
enddo
end subroutine

type :: dt(k,l)
  integer, kind :: k = 0
  integer, len :: l
  sequence
  integer(k) i(l)
end type

interface
  subroutine sub(x)
    import dt
    type(dt(2,*)) x
  end subroutine
end interface

type(dt(2,3)) a
integer*4 i

call sub(a)
do i = 1, a%l
  if (a%i(i) /= i) call zzrc(i)
enddo
end
