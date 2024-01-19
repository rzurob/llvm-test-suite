!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound, shape and size intrinsics
!*
!* DESCRIPTION                : SELECT TYPE
!* ===================================================================

implicit none

type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   character(:), pointer :: dechar(:, :)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(baseKind, baseLen1, baseLen2)), pointer :: ptr(:)
end  type

class(*), allocatable :: ptr(:)
class(*), pointer :: val
character(:), pointer :: p(:, :)
type(child(8, 5, 10, 5)) :: c
type(base(8, 5, 10)), target :: tar(2)

allocate(p(5, 8), source='xyz')
c%ptr => tar
tar(1)%dechar => p
print *, len(p),  len(tar(1)%dechar)

allocate(ptr(20), source = c%ptr(1)%dechar(1, :))
print *, lbound(ptr), ubound(ptr), size(ptr), shape(ptr)

val => c%ptr(1)%dechar(1,1)
select type (val)
   type is (character(*))
      print *, len(val)
end select

end
