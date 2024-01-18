!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyTarget04.f
!*
!*  DATE                       : Nov. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. actual argument is pointer which points to target.
!*  2. pass actual argument through several procedures, dummy arguments have intent(in) attribute, dummy argument in sub1 is polymorphic pointer with deferred length, dummy arguments in sub2 & sub3 are polymorphic target with assumed length parameter.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
       integer,len :: l1=10
       character(l1) :: firstname="no name"
   end type

   type,extends(base) :: child(l2)
       integer,len    :: l2=15
       character(l2)  :: lastname="no name"
   end type

   contains

       subroutine sub1(arg)
          class(base(:)),pointer,intent(in) :: arg(:)

          if(lbound(arg,1) /= -5)                       error stop 10_4
          if(ubound(arg,1) /= -3)                       error stop 11_4

          call sub2(arg)
       end subroutine

       subroutine sub2(arg)
          class(base(*)), target,intent(in) :: arg(2:)
          if(lbound(arg,1) /= 2)                        error stop 12_4
          if(ubound(arg,1) /= 4)                        error stop 13_4

          call sub3(arg)

       end subroutine

       subroutine sub3(arg)
          class(base(*)), target,intent(in) :: arg(:)

          select type(arg)
             type is(child(*,*))
               if(lbound(arg,1) /= 1)                   error stop 14_4
               if(ubound(arg,1) /= 3)                   error stop 15_4
               if(arg%l1 /= 15)                         error stop 16_4
               if(arg%l2 /= 20)                         error stop 17_4
               if(arg(1)%firstname /= "Nancy")          error stop 18_4
               if(arg(1)%lastname /= "Wang")            error stop 19_4

               if(arg(2)%firstname /= "David")          error stop 20_4
               if(arg(2)%lastname /= "Forster")         error stop 21_4

               if(arg(3)%firstname /= "Jim")            error stop 22_4
               if(arg(3)%lastname /= "Xia")             error stop 23_4
           class default
               error stop 100_4
         end select
       end subroutine

end module

program dummyArgDeferPolyTarget04
  use m
  implicit none

  class(base(:)),pointer     :: pbase(:)=>null()

  type(child(15,20)),target  :: tchild(3)

  tchild=[child(15,20)(firstname="Nancy",lastname="Wang"), &
          child(15,20)(firstname="David",lastname="Forster"), &
          child(15,20)(firstname="Jim",lastname="Xia") ]

  pbase(-5:)=>tchild

  call sub1(pbase)

end program
