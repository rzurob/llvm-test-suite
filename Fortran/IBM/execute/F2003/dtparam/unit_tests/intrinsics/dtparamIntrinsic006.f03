!* ===================================================================
!*
!* DATE                       : April 16, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound and size intrinsics
!*
!* DESCRIPTION                : SELECT TYPE used with DTP
!* ===================================================================
!* DEFECT 323005

implicit none
type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseLen1 * baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(4, baseLen1, baseLen2)), pointer :: len(:)
end  type

integer i
class(*), allocatable :: ptr(:)
class(*), allocatable :: val
allocate(val, source = base(4, 5, 10)(baseId = (/(i*2, i = 1, 50)/)))

select type (val)
  type is (base(4, *, *))
     val%baseId = 10
     print *, lbound(val%baseId), ubound(val%baseId), size(val%baseId)
     allocate(ptr(50), source = val%baseId)
     associate (p => ptr)
        print *, lbound(p), ubound(p), size(p), shape(p)
        print *, lbound(ptr), ubound(ptr), size(ptr), shape(ptr)
     end associate
     deallocate (ptr)
end select

end
