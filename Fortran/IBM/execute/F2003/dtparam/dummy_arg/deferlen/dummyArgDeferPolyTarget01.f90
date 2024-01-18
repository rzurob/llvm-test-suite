!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyTarget01.f   
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
!*  1. actual argument is polymorphic pointer with deferred length parameter
!*  2. pass actual argument through several subroutine with dummy argument having target and intent(inout) attribute, dummy argument in sub2 is polymorphic type with assumed length parameter, dummy argument in sub3 is child derived type,modify actual argument through dummy argument and verify result.  
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
          class(base(:)),pointer,intent(inout) :: arg
          
          call sub2(arg)
       end subroutine
 
       subroutine sub2(arg)
          class(base(*)), target,intent(inout) :: arg
          select type(arg)
            type is(child(*,*))
               if(arg%l1 /= 15)                     error stop 10_4
               if(arg%l2 /= 20)                     error stop 11_4
               if(arg%firstname /= "no name")       error stop 12_4
               if(arg%lastname /= "no name")        error stop 13_4
               arg%firstname ="James"
               arg%lastname ="Bond"
               
               call sub3(arg)

            class default
               error stop 100_4
          end select
       end subroutine

       subroutine sub3(arg)
          type(child(*,*)), target,intent(inout) :: arg

          if(arg%l1 /= 15)                         error stop 14_4
          if(arg%l2 /= 20)                         error stop 15_4
          if(arg%firstname /= "James")             error stop 16_4
          if(arg%lastname /= "Bond")               error stop 17_4
          
          arg%firstname = "007"
          arg%lastname  = "007"
       end subroutine

end module

program dummyArgDeferPolyTarget01
  use m
  implicit none

  class(base(:)),pointer     :: pbase=>null()

  type(child(:,:)),pointer   :: pchild=>null()

  type(child(15,20)),target  :: tchild 

  pchild=>tchild

  pbase=>pchild

  call sub1(pbase)

  if(.not. associated(pbase))                        error stop 18_4
  select type(pbase)
      type is(child(*,*))
        if(pbase%l1 /= 15)                           error stop 19_4
        if(pbase%l2 /= 20)                           error stop 20_4
        if(pbase%firstname /= "007")                 error stop 21_4
        if(pbase%lastname /= "007")                  error stop 22_4
      class default
        error stop 101_4
  end select

  if(tchild%firstname /= "007")                      error stop 23_4
  if(tchild%lastname /= "007")                       error stop 24_4

end program
