!*********************************************************************
!*  ===================================================================
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
!* 1. DEFECT 356494
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp
     integer(4)  :: i[1]=[1]  !<== syntax error
   end type
end module

program d356494
   use m
   implicit none

   type(dtp),parameter  :: dtp1(1)=[dtp()]

end program

