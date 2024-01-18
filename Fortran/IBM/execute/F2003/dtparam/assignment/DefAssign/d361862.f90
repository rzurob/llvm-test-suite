!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d361862.f   
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
!* 1. defect 361862
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(m)
      integer,len  :: m  ! m=4
      character(4) :: c1(4)="****"
      logical      :: g1(4)=.false.
   end type

   type :: abstype(l0)
       integer,len ::l0 ! l0=2
   end type

   type,extends(abstype) :: base(l1)
      integer,len :: l1 ! l1=3 ,l0=2
   end type

   type,extends(base) :: child(l2)
      integer,len :: l2 !l2=4
      type(A(l2)) :: a1comp(l2:l0+l1)
      type(A(l2)) :: a2comp(l2:l0+l1)
   end type

   interface assignment(=)
      module procedure assignChild,assignA 
   end interface

   contains

      elemental subroutine assignA(this,ta)
         class(A(*)),intent(inout) :: this
         class(A(*)),intent(in)    :: ta

         this%c1=ta%c1 ! call intrinsic assignment

         this%g1=ta%g1 ! call intrinsic assignment
      end subroutine

      subroutine assignChild(this,dt)
         class(child(*,*,*)),intent(inout) :: this
         class(child(*,*,*)),intent(in)    :: dt
         this%a1comp=dt%a1comp ! call assignA
         this%a2comp=dt%a2comp ! call assignA  
      end subroutine

end module

program d361862
    use m

    implicit none


    type(child(2,3,4)),allocatable,target :: child1(:,:)


    allocate(child(2,3,4) :: child1(1,1) )

    ! call assignChild
    child1(1,1) = child(2,3,4)( &
                     a1comp=[A(4)(["abcd","efgh","ijkl","mnop"], &
                                  [.true.,.false.,.true.,.false.]),&
                             A(4)(["ABCD","EFGH","IJKL","MNOP"] ,&
                                  [.false.,.true.,.false.,.true.] )] , &
                     a2comp=[A(4)(["test","team","fail","pass"], &
                                  [.false.,.true.,.true.,.false.]), &
                             A(4)(["TEST","TEAM","FAIL","PASS"], &
                                  [.true.,.false.,.false.,.true.] ) ] )

    print *,child1(1,1) 

end program

