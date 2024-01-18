!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356499.f
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
!* 1. DEFECT 356499
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp(k)
     integer,kind   :: k=2
     integer        :: i(1)=[merge(k,k,.false.)]
   end type
end module

program d356499
   use m
   implicit none

   type(dtp) :: dtp1

   if(dtp1%k /= 2)                         error stop 10_4
   if(any(dtp1%i /= 2))                    error stop 11_4

end program

