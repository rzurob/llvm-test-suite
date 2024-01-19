!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 353612
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(l)
     integer,len  :: l=2
     integer :: i(kind(l))=0
   end type
end module

program d353612
  use m
  implicit none

  type(base)  :: t

end
