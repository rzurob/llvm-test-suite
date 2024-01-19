! Merge with kind parameter, integer and real components and sequence.

type dtp (k)
  integer, kind :: k
  sequence
  integer(k) ::  i
  real :: r
end type

type (dtp(4)) :: dtp1(6) = (/dtp(4)(1, 1.0), dtp(4)(2, 2.0), dtp(4)(3, 3.0), &
                             dtp(4)(4, 4.0), dtp(4)(5, 5.0), dtp(4)(6, 6.0)/)
type (dtp(4)) :: dtp2(6) = (/dtp(4)(7, 7.0), dtp(4)(8, 8.0), dtp(4)(9, 9.0), &
                             dtp(4)(10, 10.0), dtp(4)(11, 11.0), dtp(4)(12, 12.0)/)
type (dtp(4)) dtp3(2,3), dtp4(2,3), res(2,3)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
res = merge(dtp3, dtp4, mask)

print *, res(1,1)%i, ' ', res(1,1)%r, '   ', res(1,2)%i, '  ', res(1,2)%r, &
         '    ',res(1,3)%i, '  ', res(1,3)%r
print *, res(2,1)%i, ' ', res(2,1)%r, '   ', res(2,2)%i, ' ', res(2,2)%r, &
         '    ', res(2,3)%i, ' ', res(2,3)%r
end
