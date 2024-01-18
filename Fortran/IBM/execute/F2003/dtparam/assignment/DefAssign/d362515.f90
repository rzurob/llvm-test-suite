!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362515.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 22 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. defect 362515
!234567490123456749012345674901234567490123456749012345674901234567490
module m
    type A(l1)
       integer,len  :: l1 ! l1=2
       character(l1) :: c1(l1)
       integer       :: i1(l1)
       logical,pointer :: g1(:)
    end type

    type B(l2)
       integer,len :: l2 ! l2=1
       integer,allocatable :: i2(:)
       type(A(l2+1)),allocatable :: a1comp(:) 
       type(A(:)),pointer  :: a2comp
    end type
end module

program d362515
     use m
     implicit none

     logical,target :: g1(6)=[.true.,.false.,.false.,.true.,.false.,.true.]

     type(A(:)),allocatable,target :: a2(:)
     type(B(:)),allocatable,target :: b3(:)

     a2=[A(2)(["ab","AB"],[1,2],g1(1:3)), &
         A(2)(["cd","CD"],[3,4],g1(5:6)), &
         A(2)(["ef","EF"],[5,6],g1(2:5)), &
         A(2)(["gh","GH"],[7,8],g1(6:6))]

      b3=[B(1)([-1,-2,-3],a2(1:4:2),a2(2)) , &
          B(1)([-4,-5],a2(2:4:2),a2(1)), &
          B(1)([-6],a2(4:4),a2(2)),         &
          B(1)([-7,-8,-9],a2(1:1),a2(4))]

     print *,size(b3(1)%a1comp),size(b3(2)%a1comp),&
             size(b3(3)%a1comp),size(b3(4)%a1comp)

     print *,b3(1)%i2
     print *,b3(1)%a1comp(1)%c1,b3(1)%a1comp(2)%c1
     print *,b3(1)%a1comp(1)%i1,b3(1)%a1comp(2)%i1
     print *,b3(1)%a1comp(1)%g1,b3(1)%a1comp(2)%g1
     print *,b3(1)%a2comp%c1
     print *,b3(1)%a2comp%i1
     print *,b3(1)%a2comp%g1

     print *,b3(2)%i2
     print *,b3(2)%a1comp(1)%c1,b3(2)%a1comp(2)%c1
     print *,b3(2)%a1comp(1)%i1,b3(2)%a1comp(2)%i1
     print *,b3(2)%a1comp(1)%g1,b3(2)%a1comp(2)%g1
     print *,b3(2)%a2comp%c1
     print *,b3(2)%a2comp%i1
     print *,b3(2)%a2comp%g1

     print *,b3(3)%i2
     print *,b3(3)%a1comp(1)%c1
     print *,b3(3)%a1comp(1)%i1
     print *,b3(3)%a1comp(1)%g1
     print *,b3(3)%a2comp%c1
     print *,b3(3)%a2comp%i1
     print *,b3(3)%a2comp%g1

     print *,b3(4)%i2
     print *,b3(4)%a1comp(1)%c1
     print *,b3(4)%a1comp(1)%i1
     print *,b3(4)%a1comp(1)%g1
     print *,b3(4)%a2comp%c1
     print *,b3(4)%a2comp%i1
     print *,b3(4)%a2comp%g1
     
end program
