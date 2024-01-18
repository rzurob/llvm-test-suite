!*  ============================================================================
!*
!*  TEST CASE NAME             : complexPartDesignatorF9.f
!*
!*  DATE                       : 2011-01-28
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
!*     Test the use in derived types
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   ! Base type
   type CPOINT
      complex :: x
      complex :: y
      complex :: z
   end type CPOINT

   ! Extending the base type
   type, EXTENDS(CPOINT) :: CPLANE
      complex :: dir(3)
   end type CPLANE

   type(CPLANE) :: cp1, cp2(3)

   cp1%x%RE  = 3.12345678
   cp1%x%IM  = 2.12345678
   cp1%y%RE  = 5.12345678
   cp1%y%IM  = 4.12345678
   cp1%z%RE  = 7.12345678
   cp1%z%IM  = 6.12345678
   cp1%dir%RE = (/ 3.67812345, 5.67812345, 7.67812345 /)
   cp1%dir%IM = (/ 2.67812345, 4.67812345, 6.67812345 /)
   print *,"CP1:"
   print *,"   POINT:     (",cp1%x,",",cp1%y,",",cp1%z,")"
   print *,"   DIRECTION: (",cp1%dir(1),",",cp1%dir(2),",",cp1%dir(3),")"

   cp2(:)%x = (/ (0.0,0.0), (0.0,0.0), (7.5,6.5) /)
   cp2(:)%y = (/ (0.0,0.0), (0.0,0.0), (5.5,4.5) /)
   cp2(:)%z = (/ (0.0,0.0), (0.0,0.0), (3.5,2.5) /)
   cp2(:)%dir(1) = (/ (0.0, 0.0), (0.0, 0.0), (1.5, 2.5) /)
   cp2(:)%dir(2) = (/ (0.0, 0.0), (0.0, 0.0), (2.5, 1.5) /)
   cp2(:)%dir(3) = (/ (0.0, 0.0), (0.0, 0.0), (1.5, 3.5) /)
   print *,"CP2:REAL:"
   print *,"   POINT:     (",cp2%x%RE,",",cp2%y%RE,",",cp2%z%RE,")"
   print *,"   DIRECTION: (",cp2%dir(1)%RE,",",cp2%dir(2)%RE,",",cp2%dir(3)%RE,")"
   print *,"CP2:IMAG:"
   print *,"   POINT:     (",cp2%x%IM,",",cp2%y%IM,",",cp2%z%IM,")"
   print *,"   DIRECTION: (",cp2%dir(1)%IM,",",cp2%dir(2)%IM,",",cp2%dir(3)%IM,")"
   cp2(:2)%x%RE = 7.5
   cp2(:2)%x%IM = 6.5
   cp2(:2)%y%RE = 5.5
   cp2(:2)%y%IM = 4.5
   cp2(:2)%z%RE = 3.5
   cp2(:2)%z%IM = 2.5
   cp2(:2)%dir(1)%RE = (/ 7.5, 5.5 /)
   cp2(:2)%dir(1)%IM = (/ 6.5, 4.5 /)
   cp2(:2)%dir(2)%RE = (/ 3.5, 7.5 /)
   cp2(:2)%dir(2)%IM = (/ 2.5, 6.5 /)
   cp2(:2)%dir(3)%RE = (/ 5.5, 3.5 /)
   cp2(:2)%dir(3)%IM = (/ 4.5, 2.5 /)
   print *,"CP2:"
   print *,"   POINT:     (",cp2%x,",",cp2%y,",",cp2%z,")"
   print *,"   DIRECTION: (",cp2%dir(1),",",cp2%dir(2),",",cp2%dir(3),")"

end program main
