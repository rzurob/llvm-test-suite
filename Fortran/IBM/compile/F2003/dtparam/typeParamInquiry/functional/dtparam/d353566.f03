!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 17 2008
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. DEFECT 353566
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k,l)
     integer,kind :: k
     integer(k),len :: l
     integer :: i(l%kind+l%kind)
   end type
end module

  program d353566
  use m
  implicit none

  type(base(4,2)) :: t

  end
