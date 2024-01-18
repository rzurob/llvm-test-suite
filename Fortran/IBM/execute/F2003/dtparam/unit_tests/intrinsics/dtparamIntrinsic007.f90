type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseLen1 * baseLen2)
end type

type(base(4, 10, 5)), target :: base1
type(base(4, 10, :)), pointer :: base2

base1%baseId = (/(i*i, i = 1, 50)/)
base2 => base1

print *, base1%baseId(4)
print *, base2%baseId(1)
print *, lbound(base1%baseId), ubound(base1%baseId), size(base1%baseId)
print *, lbound(base2%baseId), ubound(base2%baseId), size(base2%baseId)
print *, maxval(base2%baseId)
print *, minloc(base2%baseId, kind=4)

end
   
