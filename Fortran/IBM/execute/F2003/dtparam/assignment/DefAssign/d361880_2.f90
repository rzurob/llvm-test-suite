!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361880_2.f   
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
      character(l1),allocatable:: c1(:)
   end type

   type C(l3)
      integer,len :: l3 ! l3=2
      type(A(l3+1)),allocatable :: a1comp(:)
   end type

   interface assignment(=)
      module procedure assignC1
   end interface

   contains
       subroutine assignC1(this,dt)       
           class(C(*)),intent(inout) :: this
           class(C(*)),intent(in) :: dt

           print *,"in assignC1"
       end subroutine
end module

program d361880_2

     use m
     implicit none

     type(C(2)) :: ctar3(2)

     call allocComp(ctar3)

     contains

          subroutine allocComp(dt)
               type(C(*)),intent(inout) :: dt(:)  

                integer :: l                  
                l=lbound(dt,1)               
                allocate(dt(lbound(dt,1))%a1comp(2))
                allocate(dt(lbound(dt,1)+1)%a1comp(2))

          end subroutine

end program
