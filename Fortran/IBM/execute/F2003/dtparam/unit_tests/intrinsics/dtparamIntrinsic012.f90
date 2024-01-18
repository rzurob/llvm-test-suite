!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound, shape and size intrinsics
!*
!* DESCRIPTION                : allocate, deferred length type parameter
!* ===================================================================

implicit none

type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   real(baseKind) :: arr(baseLen1*baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(baseKind, baseLen1, baseLen2)) :: ptr(num3)
end  type

type (child(8, 5, 10, 50)) :: c

if (lbound(c%ptr, dim=1) .ne. 1) error stop 1
if (ubound(c%ptr, dim = 1) .ne. 50) error stop 2
if (size(c%ptr) .ne. 50) error stop 3
if (lbound(c%arr, dim = 1) .ne. 1) error stop 4
if (ubound(c%arr, dim = 1) .ne. 50) error stop 5
if (size(c%arr) .ne. 50) error stop 6
end
