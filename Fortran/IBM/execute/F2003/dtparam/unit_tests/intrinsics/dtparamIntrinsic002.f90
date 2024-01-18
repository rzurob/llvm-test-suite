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
implicit none

type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: baseId(baseLen1 * baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(4, baseLen1, baseLen2 )), pointer :: arr(:, :)
   integer(baseKind), allocatable :: ptr(:)
end  type

type(base(4, 10, 5)), target :: base1(2, 5)
type(child(4, 10, 5, 5)), allocatable :: ch1

allocate(ch1)

ch1%arr=>base1
allocate(ch1%ptr(50), source = base1(1,2)%baseId)
print *, lbound(ch1%arr), ubound(ch1%arr), size(ch1%arr)
print *, lbound(ch1%ptr), ubound(ch1%ptr), size(ch1%ptr)

end
   
