!*********************************************************************
!*  ===================================================================
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
!*  1. actual argument is a pointer which points to array section of target,the bound is remapped.
!*  2. pass actual argument through several subroutine and verify results
!*  3. length parameter of dummy argument in last 2 procedure are assumed,they have target attribute
!*  4. dummy argument in last procedure is fixed child type.
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
          class(base(:)),pointer,intent(inout) :: arg(:)

          if(lbound(arg,1) /= -5)                  error stop 10_4
          if(ubound(arg,1) /= -4)                  error stop 11_4

          call sub2(arg)
       end subroutine

       subroutine sub2(arg)
          class(base(*)), target,intent(inout) :: arg(2:)
          select type(arg)
            type is(child(*,*))
               if(lbound(arg,1) /= 2)              error stop 12_4
               if(ubound(arg,1) /= 3)              error stop 13_4

               call sub3(arg)

            class default
               error stop 100_4
          end select
       end subroutine

       subroutine sub3(arg)
          type(child(*,*)), target,intent(inout) :: arg(:)

          if(lbound(arg,1) /= 1)                   error stop 14_4
          if(ubound(arg,1) /= 2)                   error stop 15_4
          if(arg%l1 /= 15)                         error stop 16_4
          if(arg%l2 /= 20)                         error stop 17_4
          if(arg(1)%firstname /= "David")          error stop 18_4
          if(arg(1)%lastname /= "Forster")         error stop 19_4

          if(arg(2)%firstname /= "Jim")            error stop 20_4
          if(arg(2)%lastname /= "Xia")             error stop 21_4

          arg=(/child(arg%l1,arg%l2)(), &
                child(arg%l1,arg%l2)() /)

       end subroutine

end module

program dummyArgDeferPolyTarget03
  use m
  implicit none

  class(base(:)),pointer     :: pbase(:)=>null()

  type(child(:,:)),pointer   :: pchild(:)=>null()

  type(child(15,20)),target  :: tchild(3)

  tchild=[child(15,20)(firstname="Nancy",lastname="Wang"), &
          child(15,20)(firstname="David",lastname="Forster"), &
          child(15,20)(firstname="Jim",lastname="Xia") ]

  pchild(3:)=>tchild(2:3)

  pbase(-5:-4)=>pchild

  call sub1(pbase)

  select type(pbase)
     type is(child(*,*))
        if(lbound(pbase,1) /= -5)                 error stop 22_4
        if(ubound(pbase,1) /= -4)                 error stop 23_4

        if(pbase%l1 /= 15)                        error stop 24_4
        if(pbase%l2 /= 20)                        error stop 25_4
        if(any(pbase%firstname /= "no name"))     error stop 26_4
        if(any(pbase%lastname /= "no name"))      error stop 27_4
      class default
        error stop 101_4
  end select

  if(pchild%l1 /= 15)                             error stop 28_4
  if(pchild%l2 /= 20)                             error stop 29_4
  if(lbound(pchild,1) /= 3)                       error stop 30_4
  if(ubound(pchild,1) /= 4)                       error stop 31_4
  if(any(pchild%firstname /= "no name"))          error stop 32_4
  if(any(pchild%lastname /= "no name"))           error stop 33_4

end program
