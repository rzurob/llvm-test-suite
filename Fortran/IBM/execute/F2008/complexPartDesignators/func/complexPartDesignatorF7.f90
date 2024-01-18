!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorF7.f
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
!*     Test the use as an actual argument in an elemental function
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m

   contains

   ! Returns the real part
   real elemental function CPD_R(arrayC)
      complex, intent(in) :: arrayC
      CPD_R = arrayC%RE
      return
   end function CPD_R

   ! Returns the imaginary part
   real elemental function CPD_I(arrayC)
      complex, intent(in) :: arrayC
      CPD_I = arrayC%IM
      return
   end function CPD_I

end module m

program main

   use m

   implicit none

   integer i
   real, dimension(7) :: RES
   complex :: C_S
   complex, dimension(7) :: C
   C = (/ (1.0,0.5),   (0.75,0.3),   (0.5,0.2),    (0.0,0.1), (-0.5,0.6),    (-0.75,0.7),  (-1.0,0.9) /)

   C_S = (1.5, 0.5)
   if (C_S%RE .NE. real(C_S)) then
      print *,C_S%RE," .NE. ",real(C_S)
      ERROR STOP 101
   else if (C_S%IM .NE. imag(C_S)) then
      print *,C_S%IM," .NE. ",imag(C_S)
      ERROR STOP 201
   end if

   do i = 1, 7
      if (C(i)%RE .NE. real(C(i))) then
         print *,C(i)%RE," .NE. ",real(C(i))
         ERROR STOP 301
      else if (C(i)%IM .NE. imag(C(i))) then
         print *,C(i)%IM," .NE. ",imag(C(i))
         ERROR STOP 401
      end if
   end do

   RES = CPD_R(C)
   do i = 1, 7
      if (RES(i) .NE. real(C(i))) then
         print *,RES(i)," .NE. ",real(C(i))
         ERROR STOP 501
      end if
   end do
   RES = CPD_I(C)
   do i = 1, 7
      if (RES(i) .NE. imag(C(i))) then
         print *,RES(i)," .NE. ",imag(C(i))
         ERROR STOP 601
      end if
   end do

end program main
