integer, allocatable :: jjj(:)
integer :: kkk(5)
integer m
do m=1, 5 
  kkk(m) = m + 1 
end do
allocate(jjj, source=kkk)
print*, jjj
print *, shape(jjj)
print*, lbound(jjj)
print*, ubound(jjj)
end
