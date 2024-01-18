!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Intrinsic  with Derived Type Parameter
!*
!* PROGRAMMER                 : James Ren
!* DATE                       : April 19, 2007
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : lbound and ubound intrinsics
!*
!* ===================================================================

implicit none

type base(k, l, m)
   integer, kind :: k
   integer, len  :: l, m
   integer(k) ::  arr1(l*m)
   integer(k), allocatable ::  arr2(:)
end type

type(base(4, 10, 10)) b
call sub(b%l, b%m)

contains
subroutine sub(arg1, arg2)
   integer, intent(in) :: arg1, arg2
   integer a(10)
   type(base(k=4, l=arg1, m=arg2)) :: c
   integer i, m, n
   m = lbound(c%arr1, dim=1)
   n = ubound(c%arr1, dim=1)
   do i = m, n
      c%arr1(i) = abs(i*i - size(c%arr1))
   end do   

   allocate(c%arr2(arg1*arg2), source = c%arr1)
   if (lbound(c%arr2, dim=1) .ne. m) error stop 1
   if (ubound(c%arr2, dim=1) .ne. n) error stop 2
end  subroutine
end
