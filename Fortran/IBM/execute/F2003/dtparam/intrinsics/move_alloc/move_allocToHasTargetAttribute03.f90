!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocToHasTargetAttribute03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Oct. 2 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO) 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. IF TO HAS THE TARGET ATTRIBUTE,ANY POINTER ASSOCIATED WITH FROM ON ENTRY MOVE_ALLOC BECOMES CORRESPONDINGLY ASSOCIATED WITH TO. 
!*  3. FROM AND TO ARE POLYMORPHIC 
!*  4. DEFECT 357030
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type any(l)
     integer,len  :: l
     character(l) :: c
  end type
  type base(l1)
     integer,len  :: l1
     type(any(l1)) :: any1 
  end type
  type,extends(base) :: child(l2)
     integer,len  :: l2
     type(any(l2)) :: any2 
  end type
end module

program move_allocToHasTargetAttribute03

  use m
  implicit none

  class(base(:)),target,allocatable :: from1
  class(base(:)),pointer            :: point1=>null()
  class(base(:)),pointer            :: point2=>null()  
  class(base(:)),target,allocatable :: to1 

  type(child(3,5)) :: child1=child(3,5)(any1=any(3)(c="123"), &
                                        any2=any(5)(c="456") )

  allocate(from1,source=child1)

  point1=>from1

  point2=>point1

  call move_alloc(from=from1,to=to1)

  if(allocated(from1))                              error stop 10_4
  if(.not. allocated(to1))                          error stop 11_4

  ! After move_alloc, point1 has undefined associated status
  !if(.not. associated(point1,from1))                error stop 12_4

  if(.not. associated(point2))                      error stop 13_4     

  select type(x=>to1)
     type is(child(*,*))
         if(x%l1 /= 3)                              error stop 14_4
         if(x%l2 /= 5)                              error stop 15_4
         if(x%any1%c /= "123")                      error stop 16_4
         if(x%any2%c /= "456")                      error stop 17_4
     class default
        error stop 100_4
  end select

  select type(x=>point1)
     type is(child(*,*))
         if(x%l1 /= 3)                              error stop 18_4
         if(x%l2 /= 5)                              error stop 19_4
         if(x%any1%c /= "123")                      error stop 20_4
         if(x%any2%c /= "456")                      error stop 21_4
     class default
        error stop 101_4
  end select

  select type(x=>point2)
     type is(child(*,*))
         if(x%l1 /= 3)                              error stop 22_4
         if(x%l2 /= 5)                              error stop 23_4
         if(x%any1%c /= "123")                      error stop 24_4
         if(x%any2%c /= "456")                      error stop 25_4
     class default
        error stop 102_4
  end select

  allocate(from1,source= child1)

  if(allocated(to1)) deallocate(to1)

  allocate(to1,source=fun())

  select type(x=>to1)
     type is(child(*,*))
         if(x%l1 /= 3)                              error stop 29_4
         if(x%l2 /= 5)                              error stop 30_4
         if(x%any1%c /= "123")                      error stop 31_4
         if(x%any2%c /= "456")                      error stop 32_4
     class default
         error stop 103_4
  end select

  contains
     
     function fun()
        type(child(:,:)),allocatable  :: fun
        class(base(:)),target,allocatable :: temp

        point1=>from1
        call move_alloc(from=from1,to=temp)

        if(allocated(from1))                       error stop 26_4
        if(.not. allocated(temp))                  error stop 27_4
        if(.not. associated(point1,temp))          error stop 28_4
        select type(temp)
           type is(child(*,*)) 
               allocate(fun,source=temp)
           class default
              error stop 104_4
        end select
     end function 
end program
