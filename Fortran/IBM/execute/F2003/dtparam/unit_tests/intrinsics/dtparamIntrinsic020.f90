!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTIONS               : Testing CSHIFT intrinsic function
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none

type base(l, m)
   integer, len  :: l, m
   character(1) :: char1(l, m)
   character(:), allocatable :: char2(:, :)
end type

type(base(3, 3)) :: b
character(1) :: c(3, 3), res1(3, 3), res2(3, 3)
integer i, j, k

c = reshape((/"A","B", "C", "D", "E","F","G","H","I"/), (/3,3/))
res2 = reshape((/"C","A","B","E","F","D","G","H","I"/), (/3,3/))

b%char1 = c
allocate (b%char2(b%l, b%m), source = b%char1)
res1 = cshift(b%char2, shift=(/-1,1, 0/), dim=1)
k = 0
do i = 1, 3
   do j =1, 3
      k = k + 1
      if (res1(j, i) /= res2(j, i)) call zzrc(k)
   end do
end do

res2 = reshape((/"G","E","C","A","H","F","D","B","I"/), (/3,3/))
res1 = cshift(b%char2, shift=(/-1,1, 0/), dim=2)
k = 0
do i = 1, 3
   do j =1, 3
      k = k + 1
      if (res1(j, i) /= res2(j, i)) call zzrc(k+10)
   end do
end do
end
