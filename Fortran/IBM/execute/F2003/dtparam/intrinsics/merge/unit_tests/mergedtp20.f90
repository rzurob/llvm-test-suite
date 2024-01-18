type dtp (k)
  integer, kind :: k
  integer(k) ::  i
end type

type (dtp(4)), pointer :: dtp1(:,:), dtp2(:,:), res(:,:)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

allocate(dtp1(2,3), SOURCE = reshape((/dtp(4)(1), dtp(4)(2), dtp(4)(3), &
                                       dtp(4)(4), dtp(4)(5), dtp(4)(6)/), &
                                       (/2, 3/)))
allocate(dtp2(2,3), SOURCE = reshape((/dtp(4)(7), dtp(4)(8), dtp(4)(9), &
                                       dtp(4)(10), dtp(4)(11), dtp(4)(12)/), &
                                       (/2, 3/)))
allocate(res(2,3), SOURCE = merge(dtp1, dtp2, mask))

print *, res(1,1)%i, ' ', res(1,2)%i, '  ', res(1,3)%i
print *, res(2,1)%i, ' ', res(2,2)%i, ' ', res(2,3)%i
end
