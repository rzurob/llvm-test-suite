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
!* 1. defect 361736
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type dtp(l)
       integer,len  :: l=3

       character(l) :: c1="***"
       real         :: r1(1:1)=-9.9
   end type
end module

program d361736
     use m
     logical,external :: precision_r4

     type(dtp(:)),allocatable :: dtp4(:)

     allocate(dtp(3) :: dtp4(2:5))

     if(.not. precision_r4(dtp4%r1(1),-9.9))  stop

end program