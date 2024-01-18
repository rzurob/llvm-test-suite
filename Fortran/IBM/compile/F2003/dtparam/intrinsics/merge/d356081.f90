!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356081
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(k)
     integer(2),kind :: k=8
     integer(k)      :: i=2
  end type
end module

program d356081
   use m
   implicit none

   integer :: i
   type(A(4)),target :: a4(6)=(/A(4)(i),i=1,6/) ! wrong syntax

end program


