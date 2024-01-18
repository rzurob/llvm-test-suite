! Ensure that we don't think k is a type parameter.
type dt3(l)
  integer, len :: l
  integer k
  integer(k) i     ! Error 1
end type

end
