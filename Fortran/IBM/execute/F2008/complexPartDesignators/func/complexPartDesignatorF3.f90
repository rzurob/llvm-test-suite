!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorF3.f
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
!*     Test the use in different expressions, initialization, constant and assignment
!*     including double precision number
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   integer i
   double precision D1, D2
   ! constant expression
   complex, parameter :: complex_value = (0.75,0.5)
   complex complex_number
   real value_r, value_i
   complex, dimension(7) :: complex_number_a
   real, dimension(7) :: value_r_a, value_i_a
   complex, dimension(7) :: C
   C = (/ (1.0,0.5),   (0.75,0.3),   (0.5,0.2),    (0.0,0.1), (-0.5,0.6),    (-0.75,0.7),  (-1.0,0.9) /)

   if (complex_value%RE .NE. real(complex_value)) then
         ERROR STOP 101
   else if (complex_value%IM .NE. imag(complex_value)) then
         ERROR STOP 201
   end if

   !
   ! Scalar
   !

   ! initialization expression
   value_r = complex_value%RE
   value_i = complex_value%IM

   ! assignment expression
   complex_number%RE = complex_value%RE
   complex_number%IM = complex_value%IM

   if (value_r .NE. real(complex_value)) then
      ERROR STOP 301
   else if (value_i .NE. imag(complex_value)) then
      ERROR STOP 401
   end if

   if (complex_number%RE .NE. real(complex_value)) then
      ERROR STOP 501
   else if (complex_number%IM .NE. imag(complex_value)) then
      ERROR STOP 601
   end if

   !
   ! Array
   !

   ! initialization expression
   value_r_a = C%RE
   value_i_a = C%IM
   do i = 1, 7
      if (value_r_a(i) .NE. real(C(i))) then
         print *,value_r_a(i)," .NE. ",real(C(i))
         ERROR STOP 701
      else if (value_i_a(i) .NE. imag(C(i))) then
         print *,value_i_a(i)," .NE. ",imag(C(i))
         ERROR STOP 801
      end if
   end do

   ! assignment expression
   complex_number_a%RE = C%RE
   complex_number_a%IM = C%IM
   do i = 1, 7
      if (complex_number_a(i)%RE .NE. real(C(i))) then
         print *,complex_number_a(i)%RE," .NE. ",real(C(i))
         ERROR STOP 901
      else if (complex_number_a(i)%IM .NE. imag(C(i))) then
         print *,complex_number_a(i)%IM," .NE. ",imag(C(i))
         ERROR STOP 1001
      end if
   end do

   ! Double precision numbers
   D1 = 3.12345
   D2 = 3.56789
   complex_number = (D1, D2)
   if (complex_number%RE .NE. D1) then
      ERROR STOP 1101
   else if (complex_number%IM .NE. D2) then
      ERROR STOP 1201
   end if

end program main
