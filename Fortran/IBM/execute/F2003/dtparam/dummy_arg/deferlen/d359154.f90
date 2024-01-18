!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 20 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359154
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
       integer,len :: l1=10
       character(l1) :: firstname="firstname"
   end type

   type,extends(base) :: child(l2)
       integer,len    :: l2=15
   end type
   contains

       subroutine sub1(arg)
          class(base(:)),pointer,intent(inout) :: arg(:)
          print *,arg(-5)%firstname,arg(-4)%firstname,arg(-3)%firstname
          call sub2(arg)
       end subroutine

       subroutine sub2(arg)
          class(base(*)), target,intent(inout) :: arg(2:)
          select type(arg)
            type is(child(*,*))
               print *,arg(2)%firstname,arg(3)%firstname,arg(4)%firstname
               call sub3(arg)
            class default
               error stop 10_4
          end select
       end subroutine

       subroutine sub3(arg)
          type(child(*,*)), target,intent(inout) :: arg(:)

          print *,arg(1)%firstname,arg(2)%firstname,arg(3)%firstname
       end subroutine

end module

program d359154
  use m
  implicit none

  class(base(:)),pointer     :: pbase(:)=>null()

  type(child(15,20)),target  :: tchild(3)

  tchild=[child(15,20)(firstname="TOM"), &
          child(15,20)(firstname="JEN"), &
          child(15,20)(firstname="MIKE") ]

  pbase(-5:)=>tchild

  call sub1(pbase)

end program
