!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Intrinsic  with Derived Type Parameter
!*
!* PROGRAMMER                 : James Ren
!* DATE                       : April 16, 2007
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound and size intrinsics
!*
!* DESCRIPTION                : Pointer assignment and allocatable
!* ===================================================================
!* Defect 323017
implicit none

type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseLen1 * baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(baseKind, baseLen1, baseLen2)), pointer :: len(:)
end  type

integer i
class(*), pointer :: ptr(:)
class(*), pointer :: val
type(child(8, 5, 10, 5)) :: c
type(base(8, 5, 10)), target :: tar(10)
tar = base(8, 5, 10)(baseId = (/(i*2, i = 1, 50)/))
ptr => tar
print *, kind(tar(10)%baseId(10))
print *, lbound(ptr), ubound(ptr), size(ptr), shape(ptr)
print *, lbound(tar), ubound(tar), size(tar), shape(tar)

select type (ptr)
   type is (base(8, *, *))
      print *, lbound(ptr), ubound(ptr), size(ptr), shape(ptr)
end select
end
