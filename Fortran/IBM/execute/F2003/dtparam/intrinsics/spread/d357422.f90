!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 14 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 357422
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
program d357422
  use m
  implicit none

  integer :: i
  type(dtp(2,3))             :: dtp1
  type(dtp(2,:)),allocatable :: dtp2(:)

  dtp1=dtp(2,3)(i1=[1,2,3],i3=[-4,-5,-6,-7])
  allocate(dtp1%i2(3:7),source=(/(-i,i=1,5)/))

  dtp2=spread(source=dtp1,dim=1,ncopies=3)
  do i=1,3
    if(any(dtp2(i)%i1 /= [1,2,3]))              stop 1
    if(any(dtp2(i)%i2 /= [-1,-2,-3,-4,-5]))     stop 2
    if(any(dtp2(i)%i3 /= [-4,-5,-6,-7]))        stop 3
  end do

end program

