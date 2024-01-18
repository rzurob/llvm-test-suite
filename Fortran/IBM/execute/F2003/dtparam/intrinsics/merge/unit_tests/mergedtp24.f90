type dtp (k)
  integer, kind :: k
  character(k) :: c
end type

type (dtp(4)) :: dtp1(6) = (/dtp(4)('A'), dtp(4)('B'), dtp(4)('C'), dtp(4)('D'), &
                             dtp(4)('E'), dtp(4)('F')/)
type (dtp(4)) :: dtp2(6) = (/dtp(4)('G'), dtp(4)('H'), dtp(4)('I'), dtp(4)('J'), &
                             dtp(4)('K'), dtp(4)('L')/)
type (dtp(4)) dtp3(2,3), dtp4(2,3), res(2,3)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
res = merge(dtp3, dtp4, mask)

print *, res(1,1)%c, ' ', res(1,2)%c, ' ', res(1,3)%c
print *, res(2,1)%c, ' ', res(2,2)%c, ' ', res(2,3)%c
end
