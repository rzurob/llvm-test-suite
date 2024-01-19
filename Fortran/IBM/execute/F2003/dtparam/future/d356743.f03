!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 25 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. NULL([MOLD])
!234567890123456789012345678901234567890123456789012345678901234567890

program d356743
   implicit none

   integer,pointer :: i

   Data i /null()/

   i=>null()

end program

