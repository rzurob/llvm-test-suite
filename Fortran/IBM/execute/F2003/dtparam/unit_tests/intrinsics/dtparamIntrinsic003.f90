!* ===================================================================
!*
!* DATE                       : April 16, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound and size intrinsics
!*
!* DESCRIPTION                : Pointer assignment and allocatable
!* ===================================================================
implicit none

type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseKind:baseLen1 * baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(4, baseLen1, baseLen2)), pointer :: len(:)
end  type

integer i
type(base(4, 10, 5)), target :: base1(2)
type(child(4, 10, 5, 5)), target :: tar
type(child(4, 10, 5, :)), pointer :: ch1

ch1 => tar
ch1%len=>base1
base1(1)%baseId = (/(i*i, i = 4, 50)/)
ch1%baseId = ch1%len(1)%baseId

print *, ubound(ch1%len(1)%baseId)
print *, lbound(ch1%len(1)%baseId)
print *, size(ch1%len(1)%baseId)
print *, ubound(ch1%baseId)
print *, lbound(ch1%baseId)
print *, size(ch1%baseId)
end

