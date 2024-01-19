!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : lbound, ubound and size intrinsics
!*
!* DESCRIPTION                : Allocatable with char
!* ===================================================================
!* Defect 323795

implicit none
type base( m)
   integer, len  :: m
   character(1) :: char1(m)
   character(1), pointer :: char2
end type

type(base(:)), allocatable :: b
character(1) :: c(3)

allocate(base(3) :: b)
c = (/"A","B", "C"/)
b%char1 = c
allocate (b%char2, source = b%char1(1))
if (b%char2 .ne. "A") error stop 1
if (.not. all(b%char1 .eq. c)) error stop 2
end
