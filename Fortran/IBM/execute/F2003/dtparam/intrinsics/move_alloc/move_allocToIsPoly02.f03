!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 6 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. TO IS POLYMORPHIC ARRAY
!*  3. DEFECT 357149
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      integer(k1+k1) :: i1(l1)
   end type
   type,extends(base) :: child(k2,l2)
      integer,kind   :: k2
      integer,len    :: l2
      integer(k1+k2) :: i2(l1:l2)
   end type
end module

program move_allocToIsPoly02

  use m
  implicit none

  integer :: i,j

  class(base(2,:)),allocatable :: from1(:)
  class(*),allocatable         :: from2(:)

  class(base(2,:)),allocatable :: to1(:)
  class(*),allocatable         :: to2(:)

  allocate(child(2,3,2,5) :: from1(3:6))
  select type(from1)
    type is(child(2,*,2,*))
       do i=3,6
          from1(i)%i1=[ (j,j=1,3)]
          from1(i)%i2=(/ (j,j=3,5) /)
       end do
  end select

  allocate(from2(-5:-3),source=from1)

  allocate(child(2,3,2,5) :: to1(2:100) )
  select type(to1)
     type is(child(2,*,2,*))
        do i=2,100
            to1(i)%i1=1
            to1(i)%i2=2
        end do
  end select

  allocate(child(2,3,2,5) :: to2(100:0) ) !<= zero-size array

  call move_alloc(from1,to1)

  if(allocated(from1))                         error stop 10_4
  if(.not. allocated(to1))                     error stop 11_4
  if(lbound(to1,1) /= 3)                       error stop 12_4
  if(ubound(to1,1) /= 6)                       error stop 13_4

  select type(x=>to1)
     type is(child(2,*,2,*))
       if(x%k1 /= 2 .or. x%k2 /= 2)            error stop 14_4
       if(x%l1 /= 3 .or. x%l2 /= 5)            error stop 15_4
        do i=3,6
            if(any(x(i)%i1 /= [ (j,j=1,3)]))   error stop 16_4
            if(any(x(i)%i2 /=(/ (j,j=3,5) /)))   error stop 17_4
        end do
  end select

  call move_alloc(from2,to2)

  if(allocated(from2))                         error stop 18_4
  if(.not. allocated(to2))                     error stop 19_4
  if(lbound(to2,1) /= -5)                      error stop 20_4
  if(ubound(to2,1) /= -3)                      error stop 21_4

  select type(x=>to2)
     type is(child(2,*,2,*))
       if(x%k1 /= 2 .or. x%k2 /= 2)            error stop 22_4
       if(x%l1 /= 3 .or. x%l2 /= 5)            error stop 23_4
        do i=-5,-3
            if(any(x(i)%i1 /= [ (j,j=1,3)]))   error stop 24_4
            if(any(x(i)%i2 /=(/ (j,j=3,5) /)))   error stop 25_4
        end do
  end select

end program

