! Merge with kind parameter and derived type component which contains an
! allocatable component.

type dt (k)
  integer, kind :: k
  integer(k), allocatable :: i 
end type

type dtp (k1)
  integer, kind :: k1
  type (dt(k1)) :: d1
end type

type (dtp(4)) dtp1(6), dtp2(6), dtp3(2,3), dtp4(2,3), res(2,3)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

do j = 1,6
  allocate (dtp1(j)%d1%i, SOURCE = j)
  allocate (dtp2(j)%d1%i, SOURCE = j+6)
end do

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
res = merge(dtp3, dtp4, mask)

print *, res(1,1)%d1%i, ' ', res(1,2)%d1%i, '  ', res(1,3)%d1%i
print *, res(2,1)%d1%i, ' ', res(2,2)%d1%i, ' ', res(2,3)%d1%i
end
