type base
  integer :: iii
end type 

type(base), allocatable :: b1(:)
integer, allocatable :: jjj(:)
allocate(b1(5))
allocate(jjj, source=b1%iii)
print*, lbound(jjj)
print*, ubound(jjj)
end

