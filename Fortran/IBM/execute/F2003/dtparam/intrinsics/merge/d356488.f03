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
!* 1. DEFECT 356488
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp
     integer     :: i
   end type
end module

program d356488
   use m
   implicit none

   type(dtp),parameter:: dtp1=dtp(i=1)
   type(dtp),parameter:: dtp2=dtp(i=-1)
   type(dtp) :: dtp3=merge(dtp1,dtp2,.true.)

   if(dtp3%i /= 1)                           error stop 10_4

end program

