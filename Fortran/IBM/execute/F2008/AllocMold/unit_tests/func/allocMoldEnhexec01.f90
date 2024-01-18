integer, allocatable :: iii(:)
integer, allocatable :: jjj(:)
integer m
allocate(iii(5))
do m=1, 5 
  iii(m) = m + 1 
end do
allocate(jjj, source=iii)
print*, jjj
print *, shape(jjj)
print*, lbound(jjj)
print*, ubound(jjj)
end
