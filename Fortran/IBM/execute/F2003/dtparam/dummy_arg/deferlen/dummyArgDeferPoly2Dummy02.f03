!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  first dummy argument is unlimited polymorphic type with intent(out) attribute,its value taken from second dummy argument which has intent(in) attribute.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(l1,l2)
     integer,len :: l1,l2
     integer     :: i1(l1:l2)
  end type
  type,extends(base) :: child(k1,k2)
     integer,kind :: k1,k2
     integer      :: i2(k1:k2)
  end type

end module

program dummyArgDeferPoly2Dummy02
  use m
  implicit none


  class(base(:,:)),allocatable       :: poly(:)
  class(*),allocatable               :: upoly(:)
  type(child(:,:,2,4)),allocatable   :: tchild(:)


  allocate(poly(3:4),source= &
      [child(2,3,2,4)(i1=[1,2],i2=[3,4,5]),&
       child(2,3,2,4)(i1=[-1,-2],i2=[-3,-4,-5]) ])

  call sub1(upoly,poly)

  if(.not. allocated(upoly))                 error stop 10_4

  select type(upoly)
     type is(child(*,*,2,4))
        if(upoly%l1 /= 2)                    error stop 11_4
        if(upoly%l2 /= 3)                    error stop 12_4
        if(any(upoly(1)%i1 /= [1,2]))        error stop 13_4
        if(any(upoly(1)%i2 /= [3,4,5]))      error stop 14_4
        if(any(upoly(2)%i1 /= [-1,-2]))      error stop 15_4
        if(any(upoly(2)%i2 /= [-3,-4,-5]))   error stop 16_4
      class default
        error stop 50_4
  end select

  select type(poly)
    type is(child(*,*,2,4))
       allocate(tchild(2),source=poly)
  end select

  call sub2(upoly,tchild)

  select type(upoly)
     type is(child(*,*,2,4))
        if(upoly%l1 /= 2)                    error stop 17_4
        if(upoly%l2 /= 3)                    error stop 18_4
        if(any(upoly(1)%i1 /= [-1,-2]))      error stop 19_4
        if(any(upoly(1)%i2 /= [-3,-4,-5]))   error stop 20_4
        if(any(upoly(2)%i1 /= [1,2]))        error stop 21_4
        if(any(upoly(2)%i2 /= [3,4,5]))      error stop 22_4
      class default
        error stop 51_4
  end select

  contains
     subroutine sub1(upoly,poly)
        class(*),allocatable,intent(out)          :: upoly(:)
        class(base(:,:)),allocatable,intent(in)   :: poly(:)

        allocate(upoly(2),source=poly)
     end subroutine

     subroutine sub2(upoly,child)
        class(*),allocatable,intent(out)              :: upoly(:)
        type(child(:,:,2,4)),allocatable,intent(in)   :: child(:)

        allocate(upoly(2),source=child(2:1:-1))
     end subroutine

end program
