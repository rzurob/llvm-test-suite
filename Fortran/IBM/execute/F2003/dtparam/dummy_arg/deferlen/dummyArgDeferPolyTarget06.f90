!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyTarget06.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 19 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. polymorphic pointer derived type is associated with a target.
!*  2. pass target as actual argument through multiple procedure calls, and modify target and pointer through dummy argument 
!*  3. one of dummy argument in procedure is fixed child derived type
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

   type(child(15,20)),save,target  :: tchild(3)

   contains

       subroutine sub1
          class(base(:)),pointer :: ptr(:)=>null()

          call sub2(ptr)

          if(.not. associated(ptr))                       error stop 40_4
          if(lbound(ptr,1) /= -5)                         error stop 41_4
          if(ubound(ptr,1) /= -3)                         error stop 42_4
          if(ptr%l1 /= 15)                                error stop 43_4 
          
          select type(ptr)
             class is(child(*,*))
               if(ptr%l2 /= 20)                           error stop 44_4
               if(ptr(-5)%firstname /= "Firstname1")      error stop 45_4
               if(ptr(-5)%lastname /= "Lastname1")        error stop 46_4
               if(ptr(-4)%firstname /= "Firstname2")      error stop 47_4
               if(ptr(-4)%lastname /= "Lastname2")        error stop 48_4
               if(ptr(-3)%firstname /= "Firstname3")      error stop 49_4
               if(ptr(-3)%lastname /= "Lastname3")        error stop 50_4
             class default
               error stop 100_4
          end select
       end subroutine
 
       subroutine sub2(arg)
         class(base(:)), pointer,intent(inout) :: arg(:)


         tchild=[child(15,20)(firstname="Surname1",lastname="Familyname1"), &
                 child(15,20)(firstname="Surname2",lastname="Familyname2"), &
                 child(15,20)(firstname="Surname3",lastname="Familyname3") ]

         arg(-5:)=>tchild

         if(lbound(arg,1) /= -5)                          error stop 10_4
         if(ubound(arg,1) /= -3)                          error stop 11_4

          call sub3(tchild)

       end subroutine

       subroutine sub3(arg)
          class(child(*,*)), target,intent(inout) :: arg(2:)

          if(lbound(arg,1) /= 2)                         error stop 14_4
          if(ubound(arg,1) /= 4)                         error stop 15_4
          if(arg%l1 /= 15)                               error stop 16_4
          if(arg%l2 /= 20)                               error stop 17_4
          if(arg(2)%firstname /= "Surname1")             error stop 18_4
          if(arg(2)%lastname /= "Familyname1")           error stop 19_4

          if(arg(3)%firstname /= "Surname2")             error stop 20_4
          if(arg(3)%lastname /= "Familyname2")           error stop 21_4
          
          if(arg(4)%firstname /= "Surname3")             error stop 22_4
          if(arg(4)%lastname /= "Familyname3")           error stop 23_4
      
         call sub4(arg)

       end subroutine

       subroutine sub4(arg)
          class(base(*)), target,intent(inout) :: arg(:)
          select type(arg)
            type is(child(*,*))
              if(arg%l1 /= 15)                           error stop 24_4
              if(arg%l2 /= 20)                           error stop 25_4
              if(arg(1)%firstname /= "Surname1")         error stop 26_4
              if(arg(1)%lastname /= "Familyname1")       error stop 27_4
              if(arg(2)%firstname /= "Surname2")         error stop 28_4 
              if(arg(2)%lastname /= "Familyname2")       error stop 29_4
              if(arg(3)%firstname /= "Surname3")         error stop 30_4
              if(arg(3)%lastname /= "Familyname3")       error stop 31_4
              call sub5(arg)
             class default
              error stop 101_4
         end select
       end subroutine

       subroutine sub5(arg)
          type(child(*,*)), target,intent(inout) :: arg(-1:)

          arg(-1)%firstname = "Firstname1"
          arg(-1)%lastname  = "Lastname1"
          arg(0)%firstname  = "Firstname2"
          arg(0)%lastname   = "Lastname2"
          arg(1)%firstname  = "Firstname3"
          arg(1)%lastname   = "Lastname3"
       end subroutine

end module

program dummyArgDeferPolyTarget06
  use m
  implicit none

  call sub1

end program
