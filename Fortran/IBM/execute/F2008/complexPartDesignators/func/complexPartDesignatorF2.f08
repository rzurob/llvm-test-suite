!*  ============================================================================
!*
!*  DATE                       : 2011-01-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Test the use as an actual argument, as array, in a module,
!*     a function and a subroutine
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m

   contains

   subroutine CPD_SUB_R(c, r)
      complex, intent(in), dimension(:) :: c
      real, intent(out), dimension(:) :: r
      r = c%RE
      return
   end subroutine CPD_SUB_R

   subroutine CPD_SUB_I(c, i)
      complex, intent(in), dimension(:) :: c
      real, intent(out), dimension(:) :: i
      i = c%IM
      return
   end subroutine CPD_SUB_I

   function CPD_FUN_R(c) result (r)
      complex, intent(in), dimension(:) :: c
      real, dimension(size(c/2)) :: r
      r = c%RE
      return
   end function CPD_FUN_R

   function CPD_FUN_I(c) result (i)
      complex, intent(in), dimension(:) :: c
      real, dimension(size(c/2)) :: i
      i = c%IM
      return
   end function CPD_FUN_I

end module m

program main

   use m

   implicit none

   integer i
   complex, parameter :: complex_value = (1.1234567,0.3456789)
   real, dimension(7) :: RES
   complex, dimension(7) :: C
   C = (/ (1.0,0.5),   (0.75,0.3),   (0.5,0.2),    (0.0,0.1), (-0.5,0.6),    (-0.75,0.7),  (-1.0,0.9) /)

   !
   ! For testing real and imaginary before
   ! moving to the complex part designators
   !
   if (imag(C(1))+0.5 .NE. real(C(1))) then
      print *,imag(C(1))+0.5," .NE. ",real(C(1))
      ERROR STOP 1001
   end if

   do i = 1, 7
      if (C(i)%RE .NE. real(C(i))) then
         print *,C(i)%RE," .NE.",real(C(i))
         ERROR STOP 101
      else if (C(i)%IM .NE. imag(C(i))) then
         ERROR STOP 201
      end if
   end do

   call CPD_SUB_R(C, RES)
   do i = 1, 7
      if (RES(i) .NE. real(C(i))) then
         ERROR STOP 301
      end if
   end do
   call CPD_SUB_I(C, RES)
   do i = 1, 7
      if (RES(i) .NE. imag(C(i))) then
         ERROR STOP 401
      end if
   end do

   RES = CPD_FUN_R(C)
   do i = 1, 7
      if (RES(i) .NE. real(C(i))) then
         ERROR STOP 501
      end if
   end do
   RES = CPD_FUN_I(C)
   do i = 1, 7
      if (RES(i) .NE. imag(C(i))) then
         ERROR STOP 601
      end if
   end do

   if (complex_value%RE .NE. 1.1234567) then
         ERROR STOP 701
   else if (complex_value%IM .NE. 0.3456789) then
         ERROR STOP 801
   end if

end program main
