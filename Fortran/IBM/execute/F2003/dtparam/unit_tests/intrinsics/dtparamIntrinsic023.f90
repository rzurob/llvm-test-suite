!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Intrinsic with DTP
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit test
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing CSHIFT intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
!DEFECT 323941

implicit none

type base(l, m)
   integer, len  :: l, m
   character(1) :: char1(l, m)
   character(:), allocatable :: char2(:, :)
end type

type(base(3, :)), pointer :: b1
type(base(3, 4)), target ::  b2
character(1) :: c(3, 4)
character(:), allocatable :: ch(:, :)
integer i, j, k

c = reshape((/"A","B", "C", "D", "E","F","G","H","I", "J", "K", "L"/), (/3,4/))
b2%char1 = c
allocate (b2%char2(3, 4), source=b2%char1)
b1 => b2
print *, b1%char2
allocate(ch(b1%l, b2%m), source= b2%char2//b1%char2)


print *, cshift(ch, shift = (/-2, 2, 1, 1/), dim = 2)

end
