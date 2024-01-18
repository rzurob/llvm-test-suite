!* ===================================================================
!*
!* DATE                       : April 16, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound and size intrinsics
!*
!* DESCRIPTION                : Pointer assignment and allocatable
!* ===================================================================

type base(baseKind, baseNum1, baseNum2)
   integer, kind :: baseKind
   integer, len  :: baseNum1, baseNum2
   integer(baseKind) :: baseId(baseNum1 * baseNum2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   real :: len(num3 + baseNum2)
end  type

type(base(4, 10, 5)), target :: base1
type(base(4, :, :)), pointer :: base2

type(child(4, 5, 3, 3)) :: ch1
type(child(4, :, :, :)), allocatable :: ch2
base1%baseId = (/(i*i, i = 1, 50)/)
ch1%len = base1%baseId(1:6)
allocate(ch2, source=ch1)

base2 => base1
print *, base1%baseId(4)
print *, base2%baseId(1)
print *, lbound(base1%baseId), ubound(base1%baseId), size(base1%baseId)
print *, lbound(base2%baseId), ubound(base2%baseId), size(base2%baseId)
print *, lbound(ch1%baseId), ubound(ch1%baseId), size(ch1%baseId)
print *, lbound(ch2%baseId), ubound(ch2%baseId), size(ch2%baseId)
print *, lbound(ch2%len), ubound(ch2%len), size(ch2%len)
print *, maxval(base2%baseId)
print *, minloc(base2%baseId, kind=4)
print *, product(ch2%len)
print *, sum(ch2%len), sum(ch1%len)

end

