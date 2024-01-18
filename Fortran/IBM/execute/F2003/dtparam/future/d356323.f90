!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356323.f
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
!* 1. DEFECT 356323
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
     integer,len    :: l1
     integer(4)     :: i1
     character(l1)  :: c1
   end type
   contains
      function getDT1(dt)
         type(A(*)),intent(in)  :: dt
         type(A(dt%l1))         :: getDT1

         getDT1=dt
      end function
end module

program d356323
   use m
   implicit none

   type(A(5))      :: a3
   a3%i1=10
   a3%c1="123"

   print *,getDT1(a3)
   associate(x=>getDT1(a3))
      print *,x%i1
   end associate

end program

