!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356498.f
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
!* 1. DEFECT 356498
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp(k)
     integer,kind :: k=2
     integer      :: i(2)=[k,2] ! ok if it is i(2)=k
   end type
end module

program d356498
   use m
   implicit none

   type(dtp) :: dtp1

   if(dtp1%k /= 2)                                     error stop 10_4
   if(any(dtp1%i /= [2,2]))                            error stop 11_4

end program

