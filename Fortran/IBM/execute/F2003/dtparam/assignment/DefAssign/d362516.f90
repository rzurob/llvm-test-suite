!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 22 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362516
!234567490123456749012345674901234567490123456749012345674901234567490
module m
    type A(l1)
       integer,len  :: l1
    end type

    type B(l2)
       integer,len :: l2
       type(A(l2+1)),allocatable :: a1comp(:)
    end type
end module

program d362516
     use m
     type(A(:)),allocatable,target :: a2(:)
     type(B(1)),allocatable,target :: b3(:)

     a2=[A(2)(),A(2)(),A(2)(),A(2)()]

     if(a2%l1 /= 2)                stop 1
     if(size(a2) /= 4)             stop 2

end program
