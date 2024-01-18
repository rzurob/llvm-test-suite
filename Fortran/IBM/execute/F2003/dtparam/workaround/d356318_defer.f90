!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356318.f
!*
!*  DATE                       : Sept. 17 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. DEFECT 356318
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type A(l1)
     integer,len      :: l1
   end type
   contains
      function getDT1(dt)
         type(A(*)),intent(in)  :: dt
         type(A(dt%l1))         :: getDT1

         getDT1=dt
      end function
end module

program d356318
   use m
   implicit none

   type(A(5)),target      :: a3
   associate(x=>getDT1(a3))
     if(x%l1 /= 5)                                  error stop 10_4
   end associate

end program

