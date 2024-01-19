!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. derived type has integer and character component
!*  2. test defined assignment
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      integer(k1)  :: i1
      character(l1) :: c1
      contains
         procedure,pass :: assign1=>dtassign1
         procedure,pass :: assign2=>dtassign2
         generic :: assignment(=) => assign1,assign2
   end type

   contains

      elemental subroutine dtassign1(this,arg)
         class(dtp(2,*)),intent(inout) :: this
         type(dtp(2,*)),intent(in) :: arg
         this%i1=arg%i1+1
         this%c1=arg%c1//"Q"
      end subroutine

      elemental subroutine dtassign2(this,arg)
         class(dtp(2,*)),intent(inout) :: this
         type(dtp(4,*)),intent(in) :: arg

         this%i1=arg%i1+2
         this%c1=arg%c1//"9"
      end subroutine

end module

program dummyArgDeferNonPolyDefAssign01
  use m
  implicit none

  type(dtp(2,:)),allocatable :: LHS(:),RHS1(:)

  type(dtp(4,:)),allocatable :: rhs2(:)

  allocate(dtp(2,5) :: lhs(-1:0))

  allocate(RHS1(2:3),source=[dtp(2,4)(i1=1,c1="abcd"),dtp(2,4)(i1=2,c1="efgh")])

  allocate(RHS2(4:5),source=[dtp(4,3)(i1=3,c1="123"),dtp(4,3)(i1=4,c1="456")])

  LHS=RHS1

  if(lbound(lhs,1) /= -1)                           error stop 10_4
  if(ubound(lhs,1) /= 0)                            error stop 11_4
  if(lhs%k1 /= 2)                                   error stop 12_4
  if(lhs%l1 /= 5)                                   error stop 13_4
  if(any(lhs%i1 /= [2,3]))                          error stop 14_4
  if(any(lhs%c1 /= ["abcdQ","efghQ"]))              error stop 15_4

  LHS=RHS2

  if(lbound(lhs,1) /= -1)                           error stop 16_4
  if(ubound(lhs,1) /= 0)                            error stop 17_4
  if(lhs%k1 /= 2)                                   error stop 18_4
  if(lhs%l1 /= 5)                                   error stop 19_4
  if(any(lhs%i1 /= [5,6]))                          error stop 20_4
  if(any(lhs%c1 /= ["1239","4569"]))                error stop 21_4

end program
