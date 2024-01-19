
logical, parameter :: lp(2) = .true.

type :: dt(k)
  integer, kind :: k
  logical(kind(logical(L=lp,kind=k))) :: x(2)
end type

end

