!*********************************************************************
!*  ===================================================================
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
!* 1. defect 361880
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type B(l2)
      integer,len :: l2
      character(l2),allocatable :: c3
   end type
   type C(l3)
      integer,len :: l3
      type(B(:)),allocatable :: b1comp
   end type
end module
     use m
     implicit none

     type(C(2))  :: cobj

     type(B(1)) :: b1

     b1 = B(1)(c3="x")

     cobj=C(2)(b1comp=b1)

end program
