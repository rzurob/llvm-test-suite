!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361880_1.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 5 2009 
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
!* 1. defect 361880
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len :: l1
      character(l1),allocatable :: c1(:)
      character(:),allocatable :: c2(:)  
   end type

   type B(l2)
      integer,len :: l2
      integer,allocatable :: i1(:)
      character(l2),allocatable :: c3(:) 
   end type

   type C(l3)
      integer,len :: l3 ! l3=2
      type(A(2+1)),allocatable :: a1comp(:)
      type(B(:)),allocatable :: b1comp(:) 
   end type

end module

program d361880_1

     use m
     implicit none

     type(C(2))  :: cobj(2:3)

     cobj = [ C(2) ( a1comp = [ A(3)(c1=["cup","hat"],c2=["ab","cd","ef"]),&
                                 A(3)(c1=["AB","CD","EF"],c2=["CUP","HAT"])], &
                      b1comp = [ B(1)(i1=[1,2,3,4],c3=["x","y","z"]), &
                                 B(1)(i1=[-1,-2,-3,-4],c3=["X","Y","Z"])] ) , &
               C(2) ( a1comp = [ A(3)(c1=["ibm"],c2=["red","get"]), &
                                 A(3)(c1=["RED","GET"],c2=["IBM"])], &
                      b1comp = [ B(2)(i1=[5,6],c3=["go","do"]),&
                                 B(2)(i1=[-5,-6,-7],c3=["GO","DO","TO"])  ] ) ]
     print *,cobj(2)%a1comp(1)%c1
     print *,cobj(2)%a1comp(1)%c2
     print *,cobj(2)%a1comp(2)%c1
     print *,cobj(2)%a1comp(2)%c2

     print *,cobj(2)%b1comp(1)%i1
     print *,cobj(2)%b1comp(1)%c3
     print *,cobj(2)%b1comp(2)%i1
     print *,cobj(2)%b1comp(2)%c3

     print *,cobj(3)%a1comp(1)%c1
     print *,cobj(3)%a1comp(1)%c2
     print *,cobj(3)%a1comp(2)%c1
     print *,cobj(3)%a1comp(2)%c2

     print *,cobj(3)%b1comp(1)%i1
     print *,cobj(3)%b1comp(1)%c3
     print *,cobj(3)%b1comp(2)%i1
     print *,cobj(3)%b1comp(2)%c3

end program
