!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorF4.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-01-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Test the use in allocate and pointer assignment statements both scalar and array
!*     Error:
!*     101  if fails in scalar allocation assignment statement
!*     201  if fails in scalar pointer assignment statement
!*     301  if fails in array allocation assignment statement
!*     401  if fails in array pointer assignment statement
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

subroutine testArrayAllocation(N)

   integer, intent(in) :: N
   integer i
   complex, dimension(N) :: C
   complex, allocatable, dimension(:) :: C_A

   allocate(C_A(N))

   do i = 1, N
      C(i) = i * (1.5, 0.5)
   end do
   do i = 1, N
      C_A(i)%RE = C(i)%RE
      C_A(i)%IM = C(i)%IM
   end do
   do i = 1, N
      if (C(i) .NE. C_A(i)) then
         print *,C(i)," .NE. ",C_A(i)
         ERROR STOP 501
      end if
   end do

   deallocate(C_A)

end subroutine testArrayAllocation

subroutine testArrayPointer(N)

   integer, intent(in) :: N
   integer i
   real, pointer, dimension(:) :: ptr_r
   complex, dimension(N) :: C
   complex, pointer, dimension(:) :: ptr
   complex, target, dimension(N) :: tgt

   do i = 1, N
      C(i) = i * (1.5, 0.5)
   end do
   ptr => tgt
   do i = 1, N
      ptr(i)%RE = C(i)%RE
      ptr(i)%IM = C(i)%IM
   end do
   do i = 1, N
      if (C(i) .NE. ptr(i)) then
         print *,C(i)," .NE. ",ptr(i)
         ERROR STOP 601
      end if
   end do

   tgt = (3.5, 1.5)
   ptr_r => tgt%RE
   do i = 1, N
      if (ptr_r(i) .NE. 3.5) then
         print *,ptr_r(i)," .NE. ",3.5
         ERROR STOP 701
      end if
   end do
   ptr_r => tgt%IM
   do i = 1, N
      if (ptr_r(i) .NE. 1.5) then
         print *,ptr_r(i)," .NE. ",1.5
         ERROR STOP 801
      end if
   end do

end subroutine testArrayPointer

program main

   implicit none

   real, pointer :: p_r
   complex, allocatable :: C
   complex, pointer :: p_c
   complex, target :: t_c

   ! Scalar allocation
   allocate(C)
   C%RE = 1.5
   C%IM = 0.5
   if (C .NE. (1.5,0.5)) then
      print *,C," .NE. ",(1.5,0.5)
      ERROR STOP 101
   end if

   ! Scalar pointer
   p_c => t_c
   p_c%RE = C%RE
   p_c%IM = C%IM
   if (p_c .NE. C) then
      print *,p_c," .NE. ",C
      ERROR STOP 201
   end if

   ! Scalar pointer
   t_c = (1.75, 0.75)
   p_r => t_c%RE
   if (p_r .NE. 1.75) then
      print *,p_r," .NE. ",1.75
      ERROR STOP 301
   end if
   p_r => t_c%IM
   if (p_r .NE. 0.75) then
      print *,p_r," .NE. ",0.75
      ERROR STOP 401
   end if

   call testArrayAllocation(27)
   call testArrayPointer(27)

end program main
