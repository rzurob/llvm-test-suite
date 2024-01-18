!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362541.f   
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
!* 1. defect 362541
!234567490123456749012345674901234567490123456749012345674901234567490

module m
   type A(l1)
      integer,len  :: l1 ! l1=2
      integer,allocatable  :: i1(:)
      character(l1), allocatable :: c1(:)
   end type

   type B(l2)
      integer,len   :: l2 ! l2=1
      type(A(l2+1)) :: acomp1(1)     
      type(A(:)),pointer :: acomp2(:)
   end type

   interface assignment(=)
       module procedure assign1  
   end interface

   contains

      subroutine assign1(this,dt)
         class(*),intent(inout) :: this
         class(*),intent(in)    :: dt

         print *,"in assign1"
         select type(this)
            type is(A(*))
              select type(dt)
                  type is (A(*))
                     this%i1=dt%i1
                     this%c1=dt%c1
                  class default
                      stop 2
              end select
            type is(B(*))
              select type(dt)
                  type is (B(*))
                     do i=lbound(dt%acomp1,1),ubound(dt%acomp1,1)
                         this%acomp1(i)%i1=dt%acomp1(i)%i1
                         this%acomp1(i)%c1=dt%acomp1(i)%c1
                     end do
                     this%acomp2=>dt%acomp2
                  class default
                      stop 3
              end select
            class default
              stop 1
         end select

      end subroutine

end module

program d362541
     use m
     implicit none

     character(2),allocatable,target :: c1(:), c2(:)

     type(A(2)) :: aobj1
     type(A(2)),target :: aobj4(1)

     type(B(1)) :: bobj1

     c1=["ab","aB","Ab","AB"]
     c2=["xy","xY","Xy","XY"]

     aobj1=A(2)([1,2,3],c1)

     aobj4=A(2)([11,22,33],c2)

     bobj1=B(1)(aobj1,aobj4) 

     if(bobj1%acomp1(1)%l1 /= 2)                              stop 1
     if(any(bobj1%acomp1(1)%i1 /= [1,2,3]))                   stop 2
     if(any(bobj1%acomp1(1)%c1 /= ["ab","aB","Ab","AB"]))     stop 3

     if(bobj1%acomp2(1)%l1 /= 2)                              stop 4
     if(any(bobj1%acomp2(1)%i1 /= [11,22,33]))                stop 5
     if(any(bobj1%acomp2(1)%c1 /= ["xy","xY","Xy","XY"]))     stop 6

end program

