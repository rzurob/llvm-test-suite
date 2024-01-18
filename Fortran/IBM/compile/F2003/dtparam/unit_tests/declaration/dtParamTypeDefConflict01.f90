type dt4(k,l)
  integer, kind :: k
  integer, len :: l
end type
type, extends(dt4) :: et4(k) ! Error 1
  integer, len :: k
end type


type dt8(k)
  integer,kind :: k
  integer,kind :: k          ! Error 2
end type
end
