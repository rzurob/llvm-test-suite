!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 18 2008
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
!* 2. DEFECT 355154
!234567890123456789012345678901234567890123456789012345678901234567890

program d355154
  implicit none

  associate(x=>b%aaaaaa(1)(1))
  end associate

end
