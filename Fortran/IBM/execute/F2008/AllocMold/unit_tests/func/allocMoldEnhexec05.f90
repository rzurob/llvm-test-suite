integer, allocatable :: iii(:)
integer, allocatable :: jjj(:)
integer m, n
allocate(iii(5))
do m=1, 5
  iii(m) = m + 1
end do
n = 1
allocate(jjj, source=iii(n+1:4))
print*, jjj
print *, shape(jjj)
print*, lbound(jjj)
print*, ubound(jjj)
end
