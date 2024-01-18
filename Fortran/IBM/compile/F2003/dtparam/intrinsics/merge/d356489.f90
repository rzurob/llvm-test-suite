!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356489
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp
   end type
end module

program d356489
   use m
   implicit none

   type(dtp) :: dtp3[2]  !<=== wrong syntax

end program

