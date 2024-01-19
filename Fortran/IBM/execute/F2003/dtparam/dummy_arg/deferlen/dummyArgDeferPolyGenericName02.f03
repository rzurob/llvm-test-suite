!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. base type & child type both have character component
!*  2. type-bound procedure has nopass & private attribute
!*  3. base type & child type has same generic and nongeneric binding name
!*  4. dummy argument for type-bound procedure has different kind parameter
!*  5. test overriding & overriden binding
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind  :: k1=2
     integer,len   :: l1=2
     character(l1) :: c1="ab"
     contains
        procedure,private,nopass :: get1=>bget1
        procedure,private,nopass :: get2=>bget2
        generic :: get=>get1,get2
  end type

  type,extends(base)  :: child(l2)
     integer,len      :: l2=3
     character(l1+l2)    :: c2="hello"
     contains
       procedure,private,nopass :: get1=>cget1
       procedure,private,nopass :: get2=>cget2
       generic :: get=>get1,get2
  end type

  contains

     function bget1(arg)
       class(base(2,:)),allocatable,intent(in) :: arg
       class(base(2,:)),allocatable  :: bget1

       allocate(bget1,source=arg)
       bget1%c1="IBM"
       select type(bget1)
         type is(child(2,*,*))
           bget1%c2="CA"
         class default
           error stop 50_4
       end select
     end function

     function bget2(arg)
       class(base(4,:)),target,allocatable,intent(in) :: arg
       class(base(4,:)),pointer  :: bget2

       bget2=>arg
       bget2%c1="hello"
       select type(bget2)
         type is(child(4,*,*))
           bget2%c2="world"
         class default
           error stop 51_4
       end select
     end function

     function cget1(arg)
       class(base(2,:)),allocatable,intent(in) :: arg
       class(base(2,:)),allocatable  :: cget1

       allocate(cget1,source=arg)
     end function

     function cget2(arg)
       class(base(4,:)),target,allocatable,intent(in) :: arg
       class(base(4,:)),pointer  :: cget2

       allocate(cget2,source=arg)

     end function

end module

program dummyArgDeferPolyGenericName02
  use m
  implicit none

  type(child) :: dt

  class(base(2,:)),allocatable :: base1,result1
  class(base(4,:)),target,allocatable :: base2,result2

  allocate(base1,source=child(2,3,4)(c1="xlf",c2="test"))

  allocate(base2,source=child(4,5,10)(c1="12345",c2="abcdefghij"))

  allocate(result1,source=dt%get(base1) )

  if(result1%k1 /= 2 )                         error stop 10_4
  if(result1%l1 /= 3 )                         error stop 11_4
  if(result1%c1 /= "xlf")                      error stop 12_4
  select type(result1)
     type is(child(2,*,*))
       if(result1%l2 /= 4 )                    error stop 13_4
       if(result1%c2 /= "test")                error stop 14_4
     class default
       error stop 15_4
  end select

  allocate(result2,source=dt%get(base2) )

  if(result2%k1 /= 4 )                         error stop 15_4
  if(result2%l1 /= 5 )                         error stop 16_4
  if(result2%c1 /= "12345")                    error stop 17_4
  select type(result2)
     type is(child(4,*,*))
       if(result2%l2 /= 10)                    error stop 18_4
       if(result2%c2 /= "abcdefghij")          error stop 19_4
     class default
       error stop 20_4
  end select

  deallocate(result1)

  allocate(result1,source=dt%base%get(base1))
  if(result1%k1 /= 2 )                         error stop 21_4
  if(result1%l1 /= 3 )                         error stop 22_4
  if(result1%c1 /= "IBM")                      error stop 23_4
  select type(result1)
     type is(child(2,*,*))
       if(result1%l2 /= 4 )                    error stop 24_4
       if(result1%c2 /= "CA")                  error stop 25_4
     class default
       error stop 26_4
  end select

  deallocate(result2)
  allocate(result2,source=dt%base%get(base2) )

  if(result2%k1 /= 4 )                         error stop 27_4
  if(result2%l1 /= 5 )                         error stop 28_4
  if(result2%c1 /= "hello")                    error stop 29_4
  select type(result2)
     type is(child(4,*,*))
       if(result2%l2 /= 10)                    error stop 30_4
       if(result2%c2 /= "world")               error stop 31_4
     class default
       error stop 32_4
  end select
end program
