type dt(k)                     ! Error 1
  integer(k) i
  integer(8), kind :: k        ! Error 2
end type

type dt2(k)
  integer, kind :: k
  integer(2), kind :: k        ! Error 3
end type

type dt3(k)
  integer(2) k                 ! Error 4
  integer(4), kind :: k = 2    ! Error 5
end type

type dt4(k)
  integer(2) k                 ! Error 6
end type

type(dt(4)) x
type(dt2(4)) x2
type(dt3(4)) x3
type(dt4(4)) x4

if (kind(x%k) /= 4) then
  print *, kind(x%k)
  error stop 1_4
endif

if (kind(x2%k) /= 4) then
  print *, kind(x2%k)
  error stop 2_4
endif

if (kind(x3%k) /= 2) then
  print *, kind(x3%k)
  error stop 3_4
endif

if (x3%k /= 4) then
  error stop 4_4
endif

if (kind(x4%k) /= 2) then
  print *, kind(x4%k)
  error stop 5_4
endif

if (x4%k /= 4) then
  print *, x4%k
  error stop 6_4
endif
end
