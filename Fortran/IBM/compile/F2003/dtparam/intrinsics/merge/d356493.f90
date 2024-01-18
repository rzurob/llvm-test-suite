!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356493.f
!*
!*  DATE                       : Sept. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356493
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp(l)
     integer,len    :: l=4
     character(l)   :: c[2]=["xlf","123"] !<== wrong syntax
   end type
end module

program d356493
   use m
   implicit none

end program

