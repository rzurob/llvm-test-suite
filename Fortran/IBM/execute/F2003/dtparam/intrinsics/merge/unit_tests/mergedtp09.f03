! Merge with kind parameter and allocatable component.

type dtp (k)
  integer, kind :: k
  integer(k), allocatable :: i
end type

type (dtp(4)) dtp1(6), dtp2(6), dtp3(2,3), dtp4(2,3), res(2,3)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

do j = 1,6
  allocate (dtp1(j)%i, SOURCE = j)
  allocate (dtp2(j)%i, SOURCE = j+6)
end do

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
res = merge(dtp3, dtp4, mask)

print *, res(1,1)%i, ' ', res(1,2)%i, '  ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
end
