!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyGenericName01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 22 2008 
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
!*  1. base type & child type both have character component
!*  2. type-bound procedure has no pass attribute
!*  3. base type & child type has same generic and nongeneric binding name 
!*  4. dummy argument for type-bound procedure has different rank,argument list 
!*  5. test overriding & overriden binding
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind  :: k1=2
     integer,len   :: l1=2
     character(l1) :: c1="ab"
     contains
        procedure,nopass :: define1=>bdefine1
        procedure,nopass :: define2=>bdefine2
        procedure,nopass :: define3=>bdefine3
        generic :: define=>define1,define2,define3
  end type

  type,extends(base)  :: child(l2)
     integer,len      :: l2=3
     character(l1+l2)    :: c2="hello"
     contains
       procedure,nopass :: define1=>cdefine1
       procedure,nopass :: define2=>cdefine2
       procedure,nopass :: define3=>cdefine3
       generic :: define=>define1,define2,define3
  end type

  contains
    subroutine bdefine1(arg)
        class(base(2,:)),allocatable,intent(inout)   :: arg(:)
        if(any(arg%c1 /= ["123","456"]))           error stop 11_4 
        arg(-1)%c1="red"
        arg(0)%c1="RED" 
    end subroutine

    subroutine bdefine2(ptr,tar)
        class(base(2,:)),pointer,intent(inout):: ptr 
        class(child(2,*,*)),target,intent(in) :: tar 
        
        ptr=>tar  
    end subroutine

    subroutine bdefine3(ptr,tar)
        class(base(2,:)),pointer,intent(inout) :: ptr(:)
        class(child(2,*,*)),target,intent(in) :: tar(:)

        ptr(5:)=>tar(2:1:-1)
    end subroutine

    subroutine cdefine1(arg)
        class(base(2,:)),allocatable,intent(inout) :: arg(:)
        select type(arg)
           type is(child(2,*,*))
             if(any(arg%c2 /=["xlftest","1234567"]))   error stop 10_4
             arg(-1)%c2="abcdefg"
             arg(0)%c2="ABCDEFG"
           class default
             error stop 50_4
        end select
         
    end subroutine

    subroutine cdefine2(ptr,tar)
        class(base(2,:)),pointer,intent(inout) :: ptr 
        class(child(2,*,*)),target,intent(in) :: tar 

        ptr=>tar 
    end subroutine

    subroutine cdefine3(ptr,tar)
        class(base(2,:)),pointer,intent(inout) :: ptr(:)
        class(child(2,*,*)),target,intent(in) :: tar(:)

        ptr(10:)=>tar
    end subroutine

end module

program dummyArgDeferPolyGenericName01
  use m
  implicit none

  type(child) :: dt

  class(base(2,:)),allocatable :: base1(:)

  class(base(2,:)),pointer :: ptr1(:)=>null(),ptr2=>null()

  type(child(2,4,6)),target :: tar1(2)

  allocate(base1(-1:0),source= [child(2,3,4)(c1="123",c2="xlftest"), &
          child(2,3,4)(c1="456",c2="1234567")] )

  call dt%define(base1)
  
  call dt%base%define(base1)

  select type(base1)
     type is(child(2,*,*))
        if(any(base1%c1 /= ["red","RED"]))            error stop 12_4
        if(any(base1%c2 /= ["abcdefg","ABCDEFG"]))    error stop 13_4
     class default
        error stop 51_4
  end select
 
  tar1=[child(2,4,6)(c1="IBM",c2="CANADA"), &
        child(2,4,6)(c1="xlf",c2="compil")]

  call dt%define(ptr1,tar1)

  if(.not. associated(ptr1,tar1))                      error stop 14_4
  select type(ptr1)
    type is(child(2,*,*))
      if(lbound(ptr1,1) /= 10)                         error stop 15_4
      if(ubound(ptr1,1) /= 11)                         error stop 16_4
      if(ptr1%l1/= 4)                                  error stop 17_4
      if(ptr1%l2 /=6)                                  error stop 18_4
      if(any(ptr1%c1 /=["IBM","xlf"]))                 error stop 19_4
      if(any(ptr1%c2 /=["CANADA","compil"]))           error stop 20_4
    class default
      error stop 21_4
  end select

  nullify(ptr1)

  call dt%base%define(ptr1,tar1)

  if(.not. associated(ptr1,tar1(2:1:-1)))              error stop 22_4 

  select type(ptr1)
    type is(child(2,*,*))
      if(lbound(ptr1,1) /= 5)                          error stop 23_4
      if(ubound(ptr1,1) /= 6)                          error stop 24_4
      if(ptr1%l1/= 4)                                  error stop 25_4
      if(ptr1%l2/= 6)                                  error stop 26_4
      if(any(ptr1%c1 /=["xlf","IBM"]))                 error stop 27_4
      if(any(ptr1%c2 /=["compil","CANADA"]))           error stop 28_4
    class default
      error stop 29_4
  end select    

  if(associated(ptr2,tar1(1)))                         error stop 30_4
 
  if(associated(ptr2,tar1(2)))                         error stop 31_4 

  call dt%define(ptr2,tar1(2)) 

  if(.not. associated(ptr2,tar1(2)))                   error stop 32_4
 
  select type(ptr2) 
     type is(child(2,*,*))
       if(ptr2%l1 /= 4)                                error stop 33_4
       if(ptr2%l2 /= 6)                                error stop 34_4
       if(ptr2%c1 /= "xlf")                            error stop 35_4
       if(ptr2%c2 /= "compil")                         error stop 36_4
     class default
       error stop 52_4
  end select

  call dt%base%define(ptr2,tar1(1))

  if(.not. associated(ptr2,tar1(1)))                   error stop 37_4

  select type(ptr2)
     type is(child(2,*,*))
       if(ptr2%l1 /= 4)                                error stop 38_4
       if(ptr2%l2 /= 6)                                error stop 39_4
       if(ptr2%c1 /= "IBM")                            error stop 40_4
       if(ptr2%c2 /= "CANADA")                         error stop 41_4
     class default
       error stop 53_4
  end select        

end program
