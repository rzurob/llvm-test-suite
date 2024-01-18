! Merge with len parameters and derived type component which contains an array
! component.

type dt (l1, l2)
  integer, len :: l1, l2
  integer :: i(l1, l2)
end type

type dtp (l3, l4)
  integer, len :: l3, l4
  type(dt(l3, l4)) :: d1
end type

type (dtp(2,3)) dtp1, dtp2, res 

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

dtp1%d1%i = reshape((/1, 2, 3, 4, 5, 6/), (/2, 3/))
dtp2%d1%i = reshape((/7, 8, 9, 10, 11, 12/), (/2, 3/))
res%d1%i = merge(dtp1%d1%i, dtp2%d1%i, mask)

print *, res%d1%i(1,1), ' ', res%d1%i(1,2), '  ', res%d1%i(1,3)
print *, res%d1%i(2,1), ' ', res%d1%i(2,2), ' ', res%d1%i(2,3)
end
