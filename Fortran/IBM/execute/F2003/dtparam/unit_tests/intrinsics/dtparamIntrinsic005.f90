!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound and size intrinsics
!*
!* DESCRIPTION                : Pointer assignment and allocatable
!* ===================================================================
! DEFECT: 322933
type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseKind:baseLen1 * baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(4, baseLen1, baseLen2)), allocatable :: len(:)
end  type

type(base(4, 10, 5)) :: base1(2)
type(child(4, 10, 5, 5)) :: src(2)
type(child(4, 10, 5, :)), allocatable :: ch1(:)

allocate(ch1(2), source = src)
allocate(ch1(1)%len(2), source = base1)

base1(1)%baseId = (/(i*i, i = 4, 50)/)
ch1(1)%baseId = ch1(1)%len(1)%baseId

if (ubound(ch1(1)%len(1)%baseId, dim=1) .ne. 50) error stop 1
if (lbound(ch1(1)%len(1)%baseId, dim=1) .ne. 4) error stop 2
if (size(ch1(1)%len(1)%baseId) .ne. 47) error stop 3
if (ubound(ch1(1)%baseId, dim=1) .ne. 50) error stop 4
if (lbound(ch1(1)%baseId, dim=1) .ne. 4) error stop 5
if (size(ch1(1)%baseId) .ne. 47) error stop 6

end

