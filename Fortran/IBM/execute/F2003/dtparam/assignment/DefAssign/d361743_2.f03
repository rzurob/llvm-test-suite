!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361743
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type A(l1)
     integer,len :: l1
     integer     :: i1(2)=-99
  end type
  type B(l2)
     integer,len :: l2
     logical     :: g1(3)=.false.
     type(A(2))  :: a1comp(2:3)
  end type
  contains

     subroutine sub(arg)
         class(A(2)),intent(in) :: arg(:)

         if(any(arg(1)%i1 /= -99))         error stop 10
         if(any(arg(2)%i1 /= -99))         error stop 11

     end subroutine
end module

program d361743

  use m
  type(B(3)),allocatable :: b(:)

  allocate(b(2))

  call sub(b(1)%a1comp)

  if(any(b(1)%g1 .neqv. .false.))          error stop 12
  if(any(b(2)%g1 .neqv. .false.))          error stop 13

end program
