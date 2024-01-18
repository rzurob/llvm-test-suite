! Merge with kind parameter and mask with true and false elements.

type dtp (k)
  integer, kind :: k
  integer(k) ::  i
end type

type (dtp(4)) :: dtp1(6) = (/dtp(4)(1), dtp(4)(2), dtp(4)(3), dtp(4)(4), &
                             dtp(4)(5), dtp(4)(6)/)
type (dtp(4)) :: dtp2(6) = (/dtp(4)(7), dtp(4)(8), dtp(4)(9), dtp(4)(10), &
                             dtp(4)(11), dtp(4)(12)/)
type (dtp(4)) dtp3(2,3), dtp4(2,3)
class(*), allocatable :: res(:,:)

logical :: mask(2,3) = reshape((/.true., .false., .true., .false., .true., &
                                 .false./), (/2,3/))

dtp3 = reshape(dtp1, (/2, 3/))
dtp4 = reshape(dtp2, (/2, 3/))
allocate(res(2,3), SOURCE = merge(dtp3, dtp4, mask))

select type (d => res)
  type is (dtp(4))
    print *, d(1,1)%i, ' ', d(1,2)%i, '  ', d(1,3)%i
    print *, d(2,1)%i, ' ', d(2,2)%i, ' ', d(2,3)%i
end select

end
