!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 15 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS SCALAR
!*  3. IF SOURCE IS SCALAR, EACH ELEMENT OF THE RESULT HAS A VALUE EQUAL TO SOURCE
!*  4. IF SOURCE IS SCALAR,THE SHAPE OF RESULT IS (MAX(NCOPIES,0)
!*  5. SOURCE HAS POLYMORPHIC ARRAY COMPONENT
!*  6. DEFECT 357532
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      integer(k1)   :: i1
      character(l1) :: c1
   end type
   type,extends(base) :: child(k2,l2)
      integer(2),kind :: k2
      integer(2),len  :: l2
      character(l1+l2) :: c2(k1:k2)
   end type
   type container(k,l)
      integer,kind :: k
      integer,len  :: l
      class(base(k,l)),allocatable :: poly1(:)
   end type
end module

program spreadSourceIsScalarPolyComp02
  use m
  implicit none

  type(container(2,:)),allocatable :: contain1

  class(base(2,:)),allocatable :: poly2(:)

  allocate(container(2,1) :: contain1)

  allocate(poly2(11:12),source= &
                 [child(2,1,4,3)(i1=3,c1="a",c2=["00","11","22"]),&
                  child(2,1,4,3)(i1=5,c1="b",c2=["cc","dd","ee"]) ] )

  if(allocated(contain1)) then
     allocate(contain1%poly1(9:10),source=poly2)
  end if

  call verify(spread(contain1,1,5))

  contains
    subroutine verify(dt)
       type(container(2,*)),intent(in) :: dt(:)
       integer :: i

       do i=lbound(dt,1),ubound(dt,1)
          if(dt(i)%k /= 2)                                 error stop 10_4
          if(dt(i)%l /= 1)                                 error stop 11_4
          select type(x=>dt(i)%poly1)
             type is(child(2,*,4,*))
                if(x%k1 /= 2)                              error stop 12_4
                if(x%l1 /= 1)                              error stop 13_4
                if(x%k2 /= 4)                              error stop 14_4
                if(x%l2 /= 3)                              error stop 15_4
                if(x(9)%c1 /= "a")                         error stop 16_4
                if(x(9)%c1 /= "a")                         error stop 17_4
                if(x(10)%c1 /= "b")                        error stop 18_4
                if(x(10)%c1 /= "b")                        error stop 19_4

                if(x(9)%c1%len /= 1)                       error stop 20_4
                if(x(9)%c2%len /= 4)                       error stop 21_4
                if(lbound(x(9)%c2,1) /= 2)                 error stop 22_4
                if(ubound(x(9)%c2,1) /= 4)                 error stop 23_4

                if(x(10)%c1%len /= 1)                      error stop 24_4
                if(x(10)%c2%len /= 4)                      error stop 25_4
                if(lbound(x(10)%c2,1) /= 2)                error stop 26_4
                if(ubound(x(10)%c2,1) /= 4)                error stop 27_4

                if(any(x(9)%c2  /= ["00","11","22"]))      error stop 28_4
                if(any(x(10)%c2  /= ["cc","dd","ee"]))     error stop 29_4

                if(x(9)%i1 /= 3)                           error stop 30_4
                if(x(10)%i1 /= 5)                          error stop 31_4
             class default
                error stop 100_4
           end select
       end do

    end subroutine
end program
