!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 355172
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B(l)
      integer,len :: l=5
      character(:),pointer :: c1
      character(l)         :: c3=c2
   end type

   type :: A
     type(B(2)) :: b
   end type
end module

program d355172
  use m
  implicit none

end

