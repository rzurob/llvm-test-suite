!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : cshift intrinsics
!*
!* ===================================================================
!* DEFECT 323226

implicit none

integer i, j, k
integer, parameter :: KIND=8, BASELEN1 = MAX(1,5), LEN2 = SQRT(100.0)
type base(baseKind, baseLen1, baseLen2)
   integer, kind :: baseKind
   integer, len  :: baseLen1, baseLen2
   integer(baseKind) :: arr(baseLen1*baseLen2)
end type

type, extends(base) :: child(num3)
   integer, len :: num3
   type(base(baseKind, baseLen1, baseLen2)), pointer :: ptr
end  type

integer(kind = KIND) :: arr(50)
type(child(KIND, BASELEN1, LEN2, BASELEN1)) :: c
type(base(baseKind=KIND, baseLen1=5, baseLen2=10)), target :: tar

do i = 1, 5
   do j = 1, 10
      k = (i - 1) * 10 + j
      arr(k) = i + j
   end do
end do

tar%arr = arr
c%ptr =>tar
print *, cshift(c%ptr%arr, shift=-1)
print *, cshift(arr, shift=-1)
end
