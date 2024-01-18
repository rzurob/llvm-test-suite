!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyBasic02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 17 2008 
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
!* 1. parent type has unlimited polymorphic component
!* 2. extended type has polymorphic pointer component
!* 3. actual argument is polymorphic pointer array 
!* 4. pass actual argument through several procedure call, and modify component in procedure, verify actual argument and dummy argument result. 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1)
      integer,len  :: l1
      class(*),allocatable :: basecomp 
  end type

  type,extends(base) :: child(l2)
      integer,len  :: l2
      class(base(l1)),pointer :: childcomp(:)=>null()
  end type

  contains

    subroutine sub1(arg)

      class(base(:)),pointer,intent(out) :: arg(:) 

      if(associated(arg))                  error stop 10_4
      
      allocate(child(3,4) :: arg(2:3))

      allocate(arg(2)%basecomp,source="xlf")
      allocate(arg(3)%basecomp,source=10)
      
      select type(arg)
         type is(child(*,*))
             arg(2)%childcomp(5:)=>arg
             arg(3)%childcomp(-5:)=>arg(3:2:-1)
         class default
             error stop 100_4
      end select
      call sub2(arg)

    end subroutine

    subroutine sub2(arg)

      class(base(:)),pointer,intent(inout) :: arg(:)
     
      select type(arg)
          type is(child(*,*))
             if(lbound(arg,1) /= 2)                  error stop 11_4
             if(ubound(arg,1) /= 3)                  error stop 12_4
             select type(x=>arg(2)%basecomp)
                type is(character(*))
                   if(x /= "xlf")                    error stop 13_4
                class default
                   error stop 101_4
             end select
 
             select type(x=>arg(3)%basecomp)
                type is(integer)
                   if(x /= 10)                       error stop 14_4
                class default
                   error stop 102_4
             end select

             if(.not. associated(arg(2)%childcomp))  error stop 15_4
             if(.not. associated(arg(3)%childcomp))  error stop 16_4
             if(lbound(arg(2)%childcomp,1) /= 5)     error stop 17_4
             if(ubound(arg(2)%childcomp,1) /= 6)     error stop 18_4
             if(lbound(arg(3)%childcomp,1) /= -5)    error stop 19_4
             if(ubound(arg(3)%childcomp,1) /= -4)    error stop 20_4

             select type(x=>arg(2)%childcomp)
                type is(child(*,*))
                   if(x%l1 /= 3)                     error stop 21_4
                   if(x%l2 /= 4)                     error stop 22_4
                   if(lbound(x,1) /= 5)              error stop 23_4
                   if(ubound(x,1) /= 6)              error stop 24_4
                   select type(y=>x(5)%basecomp)
                       type is(character(*))
                          if(y /= "xlf")             error stop 25_4
                       class default
                          error stop 103_4
                   end select
                   select type(y=>x(6)%basecomp)
                       type is(integer)
                          if(y /= 10)                error stop 26_4
                       class default
                          error stop 104_4
                   end select
                 class default
                    error stop 105_4
             end select

            select type(x=>arg(3)%childcomp)
               type is(child(*,*))
                  if(x%l1 /= 3)                     error stop 27_4
                  if(x%l2 /= 4)                     error stop 28_4
                  if(lbound(x,1) /= -5)             error stop 29_4
                  if(ubound(x,1) /= -4)             error stop 30_4
                  select type(y=>x(-5)%basecomp)
                      type is(integer)
                         if(y /= 10)                error stop 31_4
                      class default
                         error stop 106_4
                  end select
                  select type(y=>x(-4)%basecomp)
                      type is(character(*))
                         if(y /= "xlf")             error stop 32_4
                      class default
                         error stop 107_4
                  end select
                class default
                   error stop 108_4
            end select

           call sub3(arg)

      end select 

    end subroutine

    subroutine sub3(arg)

      type(child(*,*)),target,intent(inout) :: arg(100:)
      
      if(lbound(arg,1) /= 100)                     error stop 33_4
      if(ubound(arg,1) /= 101)                     error stop 34_4
      
      if(allocated(arg(100)%basecomp))  deallocate(arg(100)%basecomp)
      if(allocated(arg(101)%basecomp))  deallocate(arg(101)%basecomp)

      allocate(arg(100)%basecomp,source=99)
      allocate(arg(101)%basecomp,source="123")

      arg(100)%childcomp(2:3)=>arg
      arg(101)%childcomp(-3:-2)=>arg
           
    end subroutine

end module

program dummyArgDeferPolyBasic02
  use m
  implicit none

  class(base(:)),pointer :: base1(:)=>null()

  call sub1(base1)

  select type(base1)
    type is(child(*,*))
             if(lbound(base1,1) /= 2)                   error stop 40_4
             if(ubound(base1,1) /= 3)                   error stop 41_4
             select type(x=>base1(2)%basecomp)
                type is(integer)
                   if(x /= 99)                          error stop 42_4
                class default
                   error stop 109_4
             end select

             select type(x=>base1(3)%basecomp)
                type is(character(*))
                   if(x /= "123")                       error stop 43_4
                class default
                   error stop 110_4
             end select

             if(.not. associated(base1(2)%childcomp))   error stop 44_4
             if(.not. associated(base1(3)%childcomp))   error stop 45_4
             if(lbound(base1(2)%childcomp,1) /= 2)      error stop 46_4
             if(ubound(base1(2)%childcomp,1) /= 3)      error stop 47_4
             if(lbound(base1(3)%childcomp,1) /= -3)     error stop 48_4
             if(ubound(base1(3)%childcomp,1) /= -2)     error stop 49_4

             select type(x=>base1(2)%childcomp)
                type is(child(*,*))
                   if(x%l1 /= 3)                        error stop 50_4
                   if(x%l2 /= 4)                        error stop 51_4
                   if(lbound(x,1) /= 2)                 error stop 52_4
                   if(ubound(x,1) /= 3)                 error stop 53_4
                   select type(y=>x(2)%basecomp)
                       type is(integer)
                          if(y /= 99)                   error stop 54_4
                       class default
                          error stop 111_4
                   end select
                   select type(y=>x(3)%basecomp)
                       type is(character(*))
                          if(y /= "123")                error stop 55_4
                       class default
                          error stop 112_4
                   end select
                 class default
                    error stop 113_4
             end select

            select type(x=>base1(3)%childcomp)
               type is(child(*,*))
                  if(x%l1 /= 3)                         error stop 56_4
                  if(x%l2 /= 4)                         error stop 57_4
                  if(lbound(x,1) /= -3)                 error stop 58_4
                  if(ubound(x,1) /= -2)                 error stop 59_4
                  select type(y=>x(-3)%basecomp)
                      type is(integer)
                         if(y /= 99)                    error stop 60_4
                      class default
                         error stop 114_4
                  end select
                  select type(y=>x(-2)%basecomp)
                      type is(character(*))
                         if(y /= "123")                 error stop 61_4
                      class default
                         error stop 115_4
                  end select
                class default
                   error stop 116_4
            end select
      class default
              error stop 117_4  
  end select
end program
