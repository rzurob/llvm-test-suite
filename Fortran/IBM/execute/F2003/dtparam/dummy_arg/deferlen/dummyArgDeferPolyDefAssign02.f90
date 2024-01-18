!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyDefAssign02.f
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
!*  1. base type has integer component, child type has derived type pointer.
!*  2. test defined assignment
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len  :: l1
      integer      :: i1
      contains
         procedure,pass :: assign=>assignbase
         generic :: assignment(=) => assign
   end type

   type,extends(base) :: child(l2)
      integer,len  :: l2
      type(base(l2)),pointer :: comp=>null()
      contains
        procedure,pass :: assign=>assignchild
        generic :: assignment(=) => assign
   end type

   contains

      elemental subroutine assignbase(this,arg)
         class(base(*)),intent(inout) :: this
         class(base(*)),intent(in)    :: arg
         this%i1=arg%i1-10
      end subroutine

      elemental subroutine assignchild(this,arg)
         class(child(*,*)),intent(inout) :: this
         class(base(*)),intent(in)         :: arg
         ! can't add class default since it won't allow stop or non-pure
         ! subroutine inside pure procedure
         select type(this)
            type is(child(*,*))
               select type(arg)
                    type is(child(*,*))
                    this%i1=arg%i1+10
                    this%comp=>arg%comp
               end select
         end select
      end subroutine

end module

program dummyArgDeferPolyDefAssign02
  use m
  implicit none

  class(base(4)),allocatable :: LHS(:),RHS(:)

  type(base(3)),target :: tar(2)

  tar(1)%i1=99
  tar(2)%i1=-99

  allocate(RHS(2:3),source= &
    [child(4,3)(i1=1),child(4,3)(i1=2)] )

  select type(RHS)
    type is(child(*,*))
      RHS(2)%comp=>tar(1)
      RHS(3)%comp=>tar(2)
    class default
      stop 11
  end select

  allocate(child(4,3) :: LHS(6:7))

  LHS=RHS

  if(any(LHS%i1 /= [11,12]))                 stop 1
  select type(LHS)
     type is(child(*,*))
       if(LHS(6)%comp%l1 /= 3)               stop 2
       if(LHS(7)%comp%l1 /= 3)               stop 3
       if(LHS(6)%comp%i1 /= 99)              stop 4
       if(LHS(7)%comp%i1 /= -99)             stop 5
     class default
       stop 9
  end select

  select type(LHS)
      type is(child(*,*))
          LHS%base=RHS
          if(any(LHS%base%i1 /= [-9,-8]))    stop 6
          if(LHS(6)%comp%i1 /= 99)           stop 7
          if(LHS(7)%comp%i1 /= -99)          stop 8
       class default
          stop 10
  end select

end program
