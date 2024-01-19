! Merge with kind parameters and default component initialization.

type dtp (k, n)
  integer, kind :: k, n
  integer(k) ::  i = n
end type

type (dtp(4,6)) :: dtp1(6)
type (dtp(4,6)) :: dtp2(6) = (/dtp(4,6)(7), dtp(4,6)(8), dtp(4,6)(9), dtp(4,6)(10), &
                               dtp(4,6)(11), dtp(4,6)(12)/)
type (dtp(4,6)) dtp3(2,3), dtp4(2,3), res(2,3)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
res = merge(TSOURCE=dtp3, FSOURCE=dtp4, MASK=mask)

print *, res(1,1)%i, ' ', res(1,2)%i, '  ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
end
