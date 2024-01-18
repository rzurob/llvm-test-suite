!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferDiag06.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 5 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument with deferred length 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. If the procedure is nonelemental and is referenced by a generic name or a defined operator or defined assignment, the ranks of the actual arguments and corresponding dummy arguments shall agree
!* 2. following is illegal since rank of actual argument doesn't match rank of dummy argument
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
    integer,kind :: k1=2
    integer,len  :: l1=3
    integer(k1)  :: i1
  end type

  interface sub 
     module procedure sub1,sub2
  end interface
  
  interface assignment(=)
     module procedure assig1,assig2  
  end interface

  contains

   subroutine sub1(arg)
     type(base(2,:)),allocatable :: arg(:,:)
   end subroutine

   subroutine sub2(arg)
     type(base(2,:)),pointer :: arg(:)
   end subroutine

   subroutine assig1(arg1,arg2)
      type(base(2,:)),allocatable,intent(out) :: arg1(:,:)
      integer,intent(in) :: arg2

      allocate(base(2,3) :: arg1(2,2))
      arg1%i1=arg2

   end subroutine

   subroutine assig2(arg1,arg2)
      type(base(2,:)),pointer,intent(out) :: arg1(:)
      integer,intent(in) :: arg2

      allocate(base(2,3) :: arg1(2))
      arg1%i1=arg2

   end subroutine

end module

program dummyArgDeferDiag06

  use m
  implicit none

  type(base(2,:)),dimension(:),allocatable :: base1
  type(base(2,:)),dimension(:,:),pointer   :: base2=>null() 

  call sub(base1) 
  call sub(base2)
  
  base1=1 
  base2=1

end program
