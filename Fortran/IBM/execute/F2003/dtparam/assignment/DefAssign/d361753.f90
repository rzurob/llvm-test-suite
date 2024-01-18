!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361753.f
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361753
!234567490123456749012345674901234567490123456749012345674901234567490
program d361753

   type base(l)
      integer,len :: l
      integer :: i(l)=-99
   end type

   associate(x=>getResult(base(2)()))
     if(x%l /= 2)         stop 10
     if(any(x%i /= -99))  stop 11
   end associate

   contains
      function getResult(arg)
         type(base(*)),intent(in) :: arg
         type(base(arg%l)),allocatable :: getResult

         getResult=arg
      end function
end program
