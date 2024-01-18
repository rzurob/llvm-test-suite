!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362431.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 19 2009 
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
!* 1. defect 362431
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type base(k1,l1)
      integer,kind :: k1             
      integer,len  :: l1 ! l1=2

      integer(k1)  :: i1(2:4)
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind   :: k2
      integer,len    :: l2
      integer(k2),allocatable :: i2(:)
   end type

   type,extends(child) :: gen3(k3,l3)
      integer,kind  :: k3
      integer,len   :: l3
   end type

   interface assignment(=)
      module procedure assign  
   end interface

   contains

      subroutine assign(this,dt)
         class(*),intent(inout) :: this
         class(*),intent(in)    :: dt

         print *,"in assign"
      end subroutine

end module

program d362431
   use m
   implicit none

   integer :: i
   class(*),allocatable :: obj1(:),obj2(:)  

   allocate(gen3(2,1,4,2,8,3) :: obj1(2))

   select type(x=>obj1)
       type is(gen3(2,*,4,*,8,*))
          x(1)%i1=[1,2,3]
          x(2)%i1=[4,5,6]
          x(1)%i2=[10]
          x(2)%i2=[11,12]
       class default
          stop 1
   end select

   allocate(base(2,1) :: obj2(2))

   do i=1,2
     select type(x=>obj1)
        type is(gen3(2,*,4,*,8,*))
             ! invoke assign
             select type(y=>obj2)
                type is(base(2,*))
                    y(i)=x(i)%child%base  
             end select
        class default
           stop 2
     end select
   end do

end program
