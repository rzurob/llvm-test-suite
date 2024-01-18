!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatBasic04b.f
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
!* 1. test WRITE statement with edit decriptor
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

     subroutine writegen3(arg)
       class(base(2,*)),intent(in) :: arg(:)
       write (*,*) " in writegen3"
       write (*,*)
       select type(arg)
         type is(gen3(2,*,*,*))
           write (*,'("1 : ",10l1)')   arg
           write (*,'("2 : ",10l3)')   arg
           write (*,'("3 : ",10g5.1)')   arg
           write (*,'("4 : ",10g5.1E1)') arg
         class default
           error stop 100_4
       end select
     end subroutine

     subroutine writechild(arg)
       class(child(2,*,*)),intent(in) :: arg(:)
       write (*,*)
       write (*,*)" in writechild"
       write (*,*)
       select type(arg)
         type is(child(2,*,*))
           write (*,'("1 : ",10l1)')   arg
           write (*,'("2 : ",10l3)')   arg
           write (*,'("3 : ",10g5.2)')   arg
           write (*,'("4 : ",10g5.2E1)') arg
         class default
           error stop 101_4
       end select
     end subroutine

end module

program formatBasic04b
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

  call writegen3(base2)

  select type(base2)
    type is(gen3(2,*,*,*))
       call writechild(base2%comp)
    class default
       error stop 102_4
  end select

end program
