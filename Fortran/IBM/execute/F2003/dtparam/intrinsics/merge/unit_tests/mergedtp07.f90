! Merge with kind parameters and derived type component.

type dt (k)
  integer, kind :: k
  integer(k) ::  i
end type

type dtp (k1)
  integer, kind :: k1
  type (dt(k1)) :: d1
end type

type (dtp(4)) :: dtp1(6) = (/dtp(4)(dt(4)(1)), dtp(4)(dt(4)(2)), & 
                             dtp(4)(dt(4)(3)), dtp(4)(dt(4)(4)), &
                             dtp(4)(dt(4)(5)), dtp(4)(dt(4)(6))/)
type (dtp(4)) :: dtp2(6) = (/dtp(4)(dt(4)(7)), dtp(4)(dt(4)(8)), &
                             dtp(4)(dt(4)(9)), dtp(4)(dt(4)(10)), &
                             dtp(4)(dt(4)(11)), dtp(4)(dt(4)(12))/)
type (dtp(4)) dtp3(2,3), dtp4(2,3), res(2,3)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
res = merge(dtp3, dtp4, mask)

print *, res(1,1)%d1%i, ' ', res(1,2)%d1%i, '  ', res(1,3)%d1%i
print *, res(2,1)%d1%i, ' ', res(2,2)%d1%i, ' ', res(2,3)%d1%i
end
