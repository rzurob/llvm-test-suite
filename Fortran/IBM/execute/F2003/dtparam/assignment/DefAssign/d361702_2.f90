!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361702_2.f
!*
!*  DATE                       : Feb. 9 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361702
!234567490123456749012345674901234567490123456749012345674901234567490
program d361702

   type :: A(l)
      integer,len :: l
      integer     :: i1(l)
      integer,allocatable :: i2
   end type

   type :: B
      type(A(2)) :: a1comp
   end type

end program
