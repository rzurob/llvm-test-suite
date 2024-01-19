integer, allocatable :: jjj(:)
integer :: kkk(5)
integer m, n
do m=1, 5
  kkk(m) = m + 1
end do
n = 1
allocate(jjj, mold=kkk(n+1:4))
print*, jjj
print *, shape(jjj)
print*, lbound(jjj)
print*, ubound(jjj)
end
