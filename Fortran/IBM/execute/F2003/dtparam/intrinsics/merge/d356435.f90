!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356435.f
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
!* 1. DEFECT 356435
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l)
     integer,len  :: l
     character(l) :: c1(2)
     logical      :: l1(2)
   end type
end module

program d356435
   use m
   implicit none

   type(A(:)),allocatable :: a1
   a1=A(4)(c1="xlf",l1= .true.)

   print *,a1%c1
   print *,a1%l1

end program

