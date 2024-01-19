!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 7 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test PRINT statement with with lw,gw.d gw.dEe edit decriptor
!* 2. derived type has logical ultimate component
!* 3. derived type is polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type :: base(k1,l1)
    integer,kind :: k1
    integer,len  :: l1
    logical(k1)  :: log1(l1-1:l1+1)
  end type

  type,extends(base) :: child(l2)
    integer,len  :: l2
    logical(k1)  :: log2(l1:l2)
  end type

  type,extends(child) :: gen3(l3)
    integer,len  :: l3
    type(child(k1,l3+1,l3+2)) :: comp
  end type

  contains

     subroutine printgen3(arg)
       class(base(2,*)),intent(in) :: arg(:)
       print *," in printgen3"
       print *
       select type(arg)
         type is(gen3(2,*,*,*))
           print '("1 : ",10l1)', arg
           print '("2 : ",10l3)', arg
           print '("3 : ",10g5.2)', arg
           print '("4 : ",10g5.2E1)', arg
         class default
           error stop 100_4
       end select
     end subroutine

     subroutine printchild(arg)
       class(child(2,*,*)),intent(in) :: arg(:)
       print *
       print *," in printchild"
       print *
       select type(arg)
         type is(child(2,*,*))
           print '("1 : ",10l1)', arg
           print '("2 : ",10l3)', arg
           print '("3 : ",10g5.2)', arg
           print '("4 : ",10g5.2E1)', arg
         class default
           error stop 101_4
       end select
     end subroutine

end module

program formatBasic04a
  use m
  implicit none

  class(base(2,3)),target,allocatable :: base1(:)
  class(base(2,:)),pointer :: base2(:)

  allocate(base1(-2:-1),source= &
  [gen3(2,3,4,5)(log1=[.true.,.false.,.true.],log2=[.false.,.true.], &
  comp=child(2,6,7)(log1=[.false.,.true.,.false.],log2=[.true.,.false.]) ), &
  gen3(2,3,4,5)(log1=.true.,log2=.false., &
  comp=child(2,6,7)(log1=[.true.,.false.,.true.],log2=.false.) ) ] )

  base2(0:)=> base1

  call printgen3(base2)

  select type(base2)
    type is(gen3(2,*,*,*))
       call printchild(base2%comp)
    class default
       error stop 102_4
  end select

end program
