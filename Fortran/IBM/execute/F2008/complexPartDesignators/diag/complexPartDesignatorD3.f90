!*  ============================================================================
!*
!*  DATE                       : 2011-01-17
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
!*     Test the kind of complex-part-designator for scalar and array
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   complex(4) c_4
   complex(4), dimension(7) :: ca_4
   complex(8) c_8
   complex(8), dimension(7) :: ca_8

   if (KIND(c_4%RE)  .NE. 4) ERROR STOP 101
   if (KIND(c_4%IM)  .NE. 4) ERROR STOP 102
   if (KIND(ca_4%RE) .NE. 4) ERROR STOP 103
   if (KIND(ca_4%IM) .NE. 4) ERROR STOP 104
   if (KIND(c_8%RE)  .NE. 8) ERROR STOP 201
   if (KIND(c_8%IM)  .NE. 8) ERROR STOP 202
   if (KIND(ca_8%RE) .NE. 8) ERROR STOP 203
   if (KIND(ca_8%IM) .NE. 8) ERROR STOP 204

end program main
