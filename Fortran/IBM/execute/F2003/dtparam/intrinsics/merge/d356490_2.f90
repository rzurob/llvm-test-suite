!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. DEFECT 356490_2
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp
     character(3)   :: c=merge("xlftest","123",.true.)
   end type
end module

program d356490_2

   use m
   implicit none

   type(dtp) :: dtp1=dtp()

   if(dtp1%c /= "xlf" )                        error stop 10_4

end program

