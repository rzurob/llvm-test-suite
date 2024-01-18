!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Intrinsic  with Derived Type Parameter
!*
!* PROGRAMMER                 : James Ren
!* DATE                       : April 19, 2007
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : all and abs intrinsics
!*
!* ===================================================================
implicit none

type base(k, l, m)
   integer, kind :: k
   integer, len  :: l, m
   real(k) ::  arr1(l*m)
end type

type, extends(base) :: child(n)
   integer, len :: n
end type    

type(child(k=8, l=2, m=5, n=10)) :: c
integer i, j, k
logical res
k = 0
do i = 1, c%l
   do j =1, c%m
      k = k + 1
      c%arr1(k) = 1.0/cos(real(k))
   end do   
end do   

res = all(c%arr1 .le. 3.5)
if (res .eqv. .true.) error stop 1
c%arr1 = abs(c%arr1)
if (any(c%arr1 .lt. 1.0)) error stop 2

end
