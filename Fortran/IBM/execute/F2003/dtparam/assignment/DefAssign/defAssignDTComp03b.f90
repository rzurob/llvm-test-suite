!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDTComp03b.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 3 2009 
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
!*  1. Test defined assignment with generic binding
!*  2. Use abstract type which has deferred binding,actual implementation for assignment is in extended type
!*  3. Generic binding has public attribute, specific type-bound procedure has private attribute
!*  4. Type-bound procedures are elemental subroutine 
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(m)
      integer,len  :: m  ! m=4
      character(m) :: c1(m)="****"
      logical      :: g1(m)=.false.
      contains
         private
         procedure,pass :: assignA
         generic,public :: assignment(=) => assignA
   end type
   
   type,abstract :: abstype(l0)
       integer,len ::l0 ! l0=2
        
       contains
         private
         procedure(assgn),deferred,pass :: assign
         generic,public :: assignment(=) => assign
   end type
  
   abstract interface
     elemental subroutine assgn(this,dt)
          import abstype
          class(abstype(*)),intent(inout) :: this
          class(abstype(*)),intent(in) :: dt 
      end subroutine

   end interface
    
   type,extends(abstype) :: base(l1)
      integer,len :: l1 ! l1=3 ,l0=2
      integer     :: i1(l0+l1)=-99
      contains
         private
         procedure,pass(this) :: assign=>assignBase
         generic,public :: assignment(=) => assign
   end type

   type,extends(base) :: child(l2)
      integer,len :: l2 !l2=4
      type(A(l2)) :: a1comp(l2:l0+l1)
      type(A(l2)) :: a2comp(l2:l0+l1)
      contains
         private
         procedure,pass(this) :: assign=>assignChild
         generic,public :: assignment(=)=> assign
   end type

   contains
       
      elemental subroutine assignA(this,ta)
         class(A(*)),intent(inout) :: this
         class(A(*)),intent(in)    :: ta

         this%c1=ta%c1  

         this%g1=ta%g1 

      end subroutine

      elemental subroutine assignBase(this,dt)
         class(base(*,*)),intent(inout) :: this
         class(abstype(*)),intent(in)    :: dt 

         select type(dt)
            type is(base(*,*))
               this%i1=dt%i1  
         end select
      end subroutine

      elemental subroutine assignChild(this,dt)
         class(child(*,*,*)),intent(inout) :: this
         class(abstype(*)),intent(in)    :: dt 

         select type(dt)
            type is(child(*,*,*))
                 this%base=dt%base     ! call assignBase
                 this%a1comp=dt%a1comp ! call assignA
                 this%a2comp=dt%a2comp ! call assignA
         end select
      end subroutine
      
end module

program defAssignDTComp03b
    call sub
end program

subroutine sub

    use m

    implicit none

    class(abstype(:)),pointer :: abstype1(:,:)=>null()

    type(child(2,3,4)),allocatable,target :: child1(:,:)

    type(child(2,3,4)),target :: child2(1,1)

    allocate(child1(1,1) )

    ! call assignChild
    child1=reshape(source= &
                          [child(2,3,4)(i1=[1,2,3,4,5], &
                     a1comp=[A(4)(["abcd","efgh","ijkl","mnop"], &
                                  [.true.,.false.,.true.,.false.]),&
                             A(4)(["ABCD","EFGH","IJKL","MNOP"] ,&
                                  [.false.,.true.,.false.,.true.] )] , &
                     a2comp=[A(4)(["test","team","fail","pass"], &
                                  [.false.,.true.,.true.,.false.]), &
                             A(4)(["TEST","TEAM","FAIL","PASS"], &
                                  [.true.,.false.,.false.,.true.] ) ] ) ], &  
                    shape=[1,1] )

    allocate(child(2,3,4) :: abstype1(1,1))

    abstype1=child1 ! call assignChild


    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x 
       class default
           stop 10
    end select 

    deallocate(abstype1)

    abstype1=>child1        ! call pointer assignment       

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 11
    end select
 
    abstype1=>child2        ! call pointer assignment 

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 12
    end select

    abstype1 = child1(1,1)  ! call assignChild

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 13
    end select  

    abstype1(1,1) = child1(1,1)  ! call assignChild

    select type(x=>abstype1)
       type is(child(*,*,*))
           write(*,*) x
       class default
           stop 14
    end select

end subroutine
