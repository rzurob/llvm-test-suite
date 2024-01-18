!********************************************************************* !*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignIntrinsicComp02a.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 2 2009 
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
!* 1. Test Defined Assignment with interface block
!* 2. Derived type has extended type
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type base(l1)
      integer,len :: l1 !l1=3
      character(l1)   :: c1(l1)="***"
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind :: k2 !k2=4
      integer,len  :: l2

      integer(k2)  :: i1(l1:l2)=-99
   end type

   interface assignment(=)
      module procedure assign1
      module procedure assign2
      module procedure assign3
   end interface

   contains

      subroutine assign1(dt1,dt2)
         class(base(*)),intent(inout) :: dt1 
         class(base(*)),intent(in)    :: dt2

         print *,"in assign1"
         select type(dt1)
            type is(child(*,4,*))
                select type(dt2)
                   type is(base(*))
                      dt1%c1=dt2%c1
                   type is(child(*,4,*))
                      dt1%c1=dt2%c1
                      dt1%i1=dt2%i1
                   class default
                      stop 14
                end select
            type is(base(*))
                dt1%c1=dt2%c1
            class default
               stop 13
         end select
      end subroutine

      subroutine assign2(dt1,dt2)
         class(child(*,4,*)),intent(inout) :: dt1
         class(base(*)),intent(in)  :: dt2(:)

         print *,"in assign2"
         
         dt1=dt2(lbound(dt2,1)) ! call assign1
      end subroutine


      subroutine assign3(dt1,dt2)
         class(base(*)),intent(inout) :: dt1(:)
         class(base(*)),intent(in)    :: dt2(:)

         integer :: l1,l2,i
        
         print *,"in assign3"

         l1=lbound(dt1,1)
         l2=lbound(dt2,1)

         select type(x=>dt1)
             type is(base(*))
                  do i=0,size(x)-1
                     x(l1+i)%c1="<"//dt2(l2+i)%c1(:)(2:2)//">"
                  end do
             type is(child(*,4,*))
                select type(y=>dt2)
                   type is(base(*))
                       do i=0,size(x)-1
                          x(l1+i)%c1="("//dt2(l2+i)%c1(:)(2:2)//")"
                       end do       
                   type is(child(*,4,*))
                      do i=0,size(x)-1 
                          x(l1+i)%i1=-y(l2+i)%i1
                          x(l1+i)%c1="{"//y(l2+i)%c1(:)(2:2)//"}"
                      end do
                   class default
                      stop 11
                end select 
             class default
                stop 10
         end select 
      end subroutine

end module

program defAssignIntrinsicComp02a
     use m
     implicit none

     class(base(3)),pointer :: ptr1(:)=>null()      
     type(child(3,4,:)),target,allocatable :: tar1(:)
     type(child(3,4,5)) :: child1(3)
     type(base(3)) :: base1(3)    
   
     allocate(child(3,4,5) :: ptr1(3),tar1(0:2))

     ! call assign3 
     tar1=[child(3,4,5)(c1=["XLF","IBM","XLC"],i1=[11,-12,13]) , &
           child(3,4,5)(c1=["123","456","789"],i1=[21,-22,23]), &
           child(3,4,5)(c1=["abc","def","ghi"],i1=[31,-32,33]) ] 

     ptr1=>tar1 ! intrinsic assignment 
     
     select type(x=>ptr1)
        type is(child(*,4,*))
            write(*,*) x 
        class default
           stop 12
     end select

     child1(1:3)=tar1(0:2) ! call assign3

     write(*,*) child1
 
     child1=tar1%base ! call assign3

     write(*,*) child1

     base1=tar1  ! call assign3

     write(*,*) base1

     child1%base=base1 ! call assign3
     
     write(*,*) child1

     tar1(0)=tar1(2) ! call assign1
    
     write(*,*) tar1

     tar1(0)=tar1(1)%base ! call assign1

     write(*,*) tar1

     tar1(0)=base1 ! call assign2

     select type(ptr1)
        type is(child(*,4,*))
           write(*,*) ptr1     
        class default
           stop 15
     end select

end program
