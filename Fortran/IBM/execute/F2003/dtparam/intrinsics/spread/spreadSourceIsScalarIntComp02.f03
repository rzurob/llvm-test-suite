!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 13 2008
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
!*  5. DERIVED TYPE HAS INTEGER ARRAY COMPONENT
!*  6. DEFECT 357422
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k,l)
      integer,kind :: k
      integer,len  :: l
      integer(k)   :: i1(l-1:l+1)
      integer(2*k),pointer ::i2(:)=>null()
      integer(k),allocatable :: i3(:)
   end type
end module

program spreadSourceIsScalarIntComp02

  use m
  implicit none

  integer :: i
  type(dtp(2,3))             :: dtp1
  type(dtp(2,:)),allocatable :: dtp2(:)
  type(dtp(2,:)),pointer     :: dtp3(:)

  dtp1=dtp(2,3)(i1=[1,2,3],i3=[-4,-5,-6,-7])
  allocate(dtp1%i2(3:7),source=(/(-i,i=1,5)/))

  associate(x=>spread(source=dtp1,dim=1,ncopies=33))
    if(size(x) /= 33)                                    error stop 10_4
    do i=1,33
       if(lbound(x(i)%i1,1) /= 2)                         error stop 11_4
       if(ubound(x(i)%i1,1) /= 4)                         error stop 12_4
       if(any(x(i)%i1 /= [1,2,3]))                        error stop 13_4
       if(lbound(x(i)%i2,1) /= 3)                         error stop 14_4
       if(ubound(x(i)%i2,1) /= 7)                         error stop 15_4
       if(any(x(i)%i2 /= [-1,-2,-3,-4,-5]))               error stop 16_4
       if(lbound(x(i)%i3,1) /= 1)                         error stop 17_4
       if(ubound(x(i)%i3,1) /= 4)                         error stop 18_4
       if(any(x(i)%i3 /= [-4,-5,-6,-7]))                  error stop 19_4
    end do
    dtp2=x
  end associate

  if(size(dtp2,1) /= 33)                                  error stop 20_4
  if(dtp2%l /= 3 .or. dtp2%k /= 2)                        error stop 21_4

  do i=1,33
    if(lbound(dtp2(i)%i1,1) /= 2)                         error stop 22_4
    if(ubound(dtp2(i)%i1,1) /= 4)                         error stop 23_4
    if(any(dtp2(i)%i1 /= [1,2,3]))                        error stop 24_4
    if(lbound(dtp2(i)%i2,1) /= 3)                         error stop 25_4
    if(ubound(dtp2(i)%i2,1) /= 7)                         error stop 26_4
    if(any(dtp2(i)%i2 /= [-1,-2,-3,-4,-5]))               error stop 27_4
    if(lbound(dtp2(i)%i3,1) /= 1)                         error stop 28_4
    if(ubound(dtp2(i)%i3,1) /= 4)                         error stop 29_4
    if(any(dtp2(i)%i3 /= [-4,-5,-6,-7]))                  error stop 30_4
  end do

  allocate(dtp3(3:6),source=spread(source=dtp2(3),dim=1,ncopies=4))

  if(size(dtp3,1) /= 4)                                   error stop 31_4
  if(dtp3%l /= 3 .or. dtp3%k /= 2)                        error stop 32_4

  do i=3,6
    if(lbound(dtp3(i)%i1,1) /= 2)                         error stop 33_4
    if(ubound(dtp3(i)%i1,1) /= 4)                         error stop 34_4
    if(any(dtp3(i)%i1 /= [1,2,3]))                        error stop 35_4
    if(lbound(dtp3(i)%i2,1) /= 3)                         error stop 36_4
    if(ubound(dtp3(i)%i2,1) /= 7)                         error stop 37_4
    if(any(dtp3(i)%i2 /= [-1,-2,-3,-4,-5]))               error stop 38_4
    if(lbound(dtp3(i)%i3,1) /= 1)                         error stop 39_4
    if(ubound(dtp3(i)%i3,1) /= 4)                         error stop 40_4
    if(any(dtp3(i)%i3 /= [-4,-5,-6,-7]))                  error stop 41_4
  end do

  do i=3,6
     associate(x=>spread(dtp3(i)%i1(2),1,i))
       if(any(x /= 1))                                    error stop 42_4
       if(size(x,1) /= i)                                 error stop 43_4
     end associate
     associate(x=>spread(dtp3(i)%i1(3),1,i))
       if(any(x /= 2))                                    error stop 44_4
       if(size(x,1) /= i)                                 error stop 45_4
     end associate
     associate(x=>spread(dtp3(i)%i1(4),1,i))
       if(any(x /= 3))                                    error stop 46_4
       if(size(x,1) /= i)                                 error stop 47_4
     end associate

     associate(x=>spread(dtp3(i)%i2(3),1,i))
       if(any(x /= -1))                                   error stop 48_4
       if(size(x,1) /= i)                                 error stop 49_4
     end associate
     associate(x=>spread(dtp3(i)%i2(7),1,i))
       if(any(x /= -5))                                   error stop 50_4
       if(size(x,1) /= i)                                 error stop 51_4
     end associate

     associate(x=>spread(dtp3(i)%i3(2),1,i))
       if(any(x /= -5))                                   error stop 52_4
       if(size(x,1) /= i)                                 error stop 53_4
     end associate
     associate(x=>spread(dtp3(i)%i3(3),1,i))
       if(any(x /= -6))                                   error stop 54_4
       if(size(x,1) /= i)                                 error stop 55_4
     end associate

  end do

end program
