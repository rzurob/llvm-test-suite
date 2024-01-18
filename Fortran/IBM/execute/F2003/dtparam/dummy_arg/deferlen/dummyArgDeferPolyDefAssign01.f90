!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyDefAssign01.f
!*
!*  DATE                       : Nov. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. base type has integer and character component, child type has no component.
!* 2. test defined assignment
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      integer(k1)  :: i1
      character(4) :: c1
      contains
         procedure,pass :: assign=>assignbase
         generic :: assignment(=) => assign
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind :: k2
      integer,len  :: l2
      contains
        procedure,pass :: assign=>assignchild
        generic :: assignment(=) => assign
   end type

   contains

      elemental subroutine assignbase(this,arg)
         class(base(2,*)),intent(inout) :: this
         class(base(2,*)),intent(in)    :: arg
         this%i1=arg%i1-10
         this%c1=arg%c1(1:2)
      end subroutine

      elemental subroutine assignchild(this,arg)
         class(child(2,*,2,*)),intent(inout) :: this
         class(base(2,*)),intent(in)         :: arg
         ! can't add class default since it won't allow stop or nonpure subroutine in pure subroutine
         select type(this)
            type is(child(2,*,2,*))
               select type(arg)
                    type is(child(2,*,2,*))
                        this%i1=arg%i1+10
                        this%c1=arg%c1(3:4)
               end select
         end select
      end subroutine

end module

program dummyArgDeferPolyDefAssign01
  use m
  implicit none

  class(base(2,4)),allocatable :: LHS(:),RHS(:)

  allocate(RHS(2:3),source= &
    [child(2,4,2,3)(i1=1,c1="ABCD"),child(2,4,2,3)(i1=2,c1="EFGH")] )

  allocate(child(2,4,2,3) :: LHS(6:7))

  LHS=RHS
  if(any(LHS%i1 /= [11,12]))                 error stop 1_4
  if(any(LHS%c1 /= ["CD","GH"]))             error stop 2_4

  select type(LHS)
    type is(child(2,*,2,*))
      LHS(6)%base=RHS(2)

      if(LHS(6)%i1 /= -9)                    error stop 3_4
      if(LHS(6)%c1 /= "AB")                  error stop 4_4

      LHS(7)%base=RHS(3)

      if(LHS(7)%i1 /= -8)                    error stop 5_4
      if(LHS(7)%c1 /= "EF")                  error stop 6_4
    class default
      stop 15
  end select

  LHS(6)=RHS(2)

  if(LHS(6)%i1 /= 11)                        error stop 7_4
  if(LHS(6)%c1 /= "CD")                      error stop 8_4

  LHS(7)=RHS(3)

  if(LHS(7)%i1 /= 12)                        error stop 9_4
  if(LHS(7)%c1 /= "GH")                      error stop 10_4

  select type(LHS)
      type is(child(2,*,2,*))
          LHS=RHS
          if(any(LHS%i1 /= [11,12]))         error stop 11_4
          if(any(LHS%c1 /= ["CD","GH"]))     error stop 12_4
      class default
          error stop 52_4
  end select

end program
