!********************************************************************* !*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignIntrinsicComp02b.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 1 2009 
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
!* 1. Test Defined Assignment with generic binding
!* 2. Derived type is polymorphic and contains intrinsic type
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type base(l1)
      integer,len :: l1 !l1=3
      character(l1)   :: c1(l1)="***"
      contains
          procedure,pass :: assign=>assign_b
          generic :: assignment(=) =>assign
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind :: k2 !k2=4
      integer,len  :: l2

      integer(k2)  :: i1(l1:l2)=-99
      contains
          procedure,pass :: assign=>assign_c
          generic :: assignment(=) =>assign
   end type

   contains

      subroutine assign_b(this,dt)
         class(base(*)),intent(inout) :: this
         class(base(*)),intent(in) :: dt

         print *,"in assign_b"
         this%c1=dt%c1
      end subroutine

      subroutine assign_c(this,dt)
         class(child(*,4,*)),intent(inout) :: this
         class(base(*)),intent(in) :: dt

         print *,"in  assign_c"
         
         this%c1=dt%c1(ubound(dt%c1,1):lbound(dt%c1,1):-1)
         select type(dt)
            type is(child(*,4,*))
               this%i1=-dt%i1
            type is(base(*))
            class default
               stop 10
         end select
      end subroutine

      function getResult(arg)
         class(base(*)),intent(in) :: arg
         class(base(arg%l1)),allocatable :: getResult

         print *,"in getResult"
         allocate(getResult)

         getResult=arg  ! call assign_b
      end function
end module

program defAssignIntrinsicComp02b
     use m
     implicit none


     class(base(3)),allocatable :: base1
     class(base(:)),allocatable :: base2
     type(child(3,4,5)) :: child1

     allocate(base1,source=base(3)(c1=["abc","def","ghi"]))

     ! call assign_c
     child1=child(3,4,5)(c1=["XLF","IBM","XLC"],i1=[11,-12,13] ) 

     write(*,*) child1

     allocate(base2,source=child1)

     base2=getResult(child1) ! call assign_c

     select type(x=>base2)
        type is(child(*,4,*))
           write(*,*) x%c1,x%i1
        class default
           stop 11
     end select

     base2=getResult(base1) ! call assign_c

     select type(x=>base2)
        type is(child(*,4,*))
           write(*,*) x%c1,x%i1
        class default
           stop 12
     end select

     base1=base(3)(c1=["RED","CUP","HAT"]) ! call assign_b

     select type(x=>base1)
        type is(base(*))
           write(*,*) x%c1
        class default
           stop 13
     end select

end program
