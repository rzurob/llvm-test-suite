module m

type dt(k)
  integer, kind :: k
  integer, len :: k    ! Error 1
end type
type(dt(4)) a

type dt2
  integer, kind :: k   ! Error 2
  integer, len :: l    ! Error 3
end type

type dt3(k,l)
  real, kind :: k      ! Error 4
  real, len :: l       ! Error 5
end type

integer, kind :: k     ! Error 6
integer, len :: l      ! Error 7

type dt4(k)
  integer, allocatable, kind :: k  ! Error 8
end type

type dt5(k)
  integer, pointer, kind :: k      ! Error 9
end type

type dt6(l)
  integer, dimension(5), len :: l  ! Error 10
end type

type dt7(k)
  integer, private, kind :: k      ! Error 11
end type

type dt8(l)
  integer, len, public :: l        ! Error 12
end type

type dt9(k)
  integer, kind, allocatable :: k  ! Error 13
end type

type dt10(k)
  integer, kind, pointer :: k      ! Error 14
end type

type dt11(l)
  integer, len, dimension(5) :: l  ! Error 15
end type

type dt12(k)
  integer, kind, private :: k      ! Error 16
end type

type dt13(l)
  integer, public, len :: l        ! Error 17
end type

type dt14(k)
  integer, kind :: k(2)            ! Error 18
end type

end module
