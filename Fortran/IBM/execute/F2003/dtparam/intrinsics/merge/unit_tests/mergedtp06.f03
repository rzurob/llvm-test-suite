! Merge with len parameters and array component.

type dtp (l1, l2)
  integer, len :: l1, l2
  integer :: i(l1, l2)
end type

type (dtp(2,3)) :: dtp1 = dtp(2,3)(reshape((/1, 2, 3, 4, 5, 6/), (/2, 3/)))
type (dtp(2,3)) :: dtp2 = dtp(2,3)(reshape((/7, 8, 9, 10, 11, 12/), (/2, 3/)))
type (dtp(2,3)) res

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

res%i = merge(dtp1%i, dtp2%i, mask)

print *, res%i(1,1), ' ', res%i(1,2), '  ', res%i(1,3)
print *, res%i(2,1), ' ', res%i(2,2), ' ', res%i(2,3)
end
