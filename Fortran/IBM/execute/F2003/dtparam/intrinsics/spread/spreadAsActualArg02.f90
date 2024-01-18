!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadAsActualArg01.f
!*
!*  DATE                       : Oct. 23 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. USE GENERIC BINDING WITH SPREAD AS ACTUAL ARGUMENT
!*  3  USE ELEMENTAL PROCEDURE
!*  4. KIND TYPE PARAMETER IS DIFFERENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,k2,l1,l2)
     integer,kind     :: k1,k2
     integer,len      :: l1,l2
     integer(k1+k2)   :: i(l1:l2)
     contains
        procedure,nopass :: spread1=>getSpread1
        procedure,nopass :: spread2=>getSpread2
        procedure,nopass :: spread3=>getSpread3
        generic :: spread=>spread1,spread2,spread3
  end type

  contains

    elemental function getSpread1(dt)
       type(dtp(2,2,*,*)),intent(in) :: dt
       type(dtp(2,2,2,3))   :: getSpread1

       getSpread1= dt
    end function

    elemental function getSpread2(dt)
       type(dtp(4,4,*,*)),intent(in) :: dt
       type(dtp(4,4,-2,0))  :: getSpread2

       getSpread2=dt
    end function

    elemental function getSpread3(dt)
       type(dtp(1,1,*,*)),intent(in) :: dt
       type(dtp(1,1,0,2))  :: getSpread3

       getSpread3=dt
    end function

end module

program spreadAsActualArg01
  use m
  implicit none

  integer :: i
  type(dtp(2,2,:,:)),allocatable :: dtp1,dtp1_0
  type(dtp(2,2,:,:)),allocatable :: dtp1_1(:)
  type(dtp(2,2,:,:)),allocatable :: dtp1_2(:,:)

  type(dtp(4,4,:,:)),allocatable :: dtp2,dtp2_0
  type(dtp(4,4,:,:)),allocatable :: dtp2_1(:)
  type(dtp(4,4,:,:)),allocatable :: dtp2_2(:,:)

  type(dtp(1,1,:,:)),allocatable :: dtp3,dtp3_0
  type(dtp(1,1,:,:)),allocatable :: dtp3_1(:)
  type(dtp(1,1,:,:)),allocatable :: dtp3_2(:,:)

  integer :: dim

  allocate(dtp(2,2,1,2) :: dtp1)
  dtp1%i=[1,2]

  allocate(dtp(4,4,-2,0) :: dtp2)
  dtp2%i=[-1,-2,-3]

  allocate(dtp(1,1,0,2) :: dtp3)
  dtp3%i=[4,5,6]

  dtp1_1=dtp1%spread(spread(dtp1,1,2))
  if(dtp1_1%k1 /= 2 .or. dtp1_1%k2 /= 2)                    error stop 10_4
  if(dtp1_1%l1 /= 2 .or. dtp1_1%l2 /= 3)                    error stop 11_4
  if(size(dtp1_1,1) /= 2)                                   error stop 12_4
  do i=1,2
     if(any(dtp1_1(i)%i /= [1,2]))                          error stop 13_4
  end do

  dtp2_1=dtp2%spread(spread(dtp2,1,2))
  if(dtp2_1%k1 /= 4 .or. dtp2_1%k2 /= 4)                    error stop 14_4
  if(dtp2_1%l1 /= -2 .or. dtp2_1%l2 /= 0)                   error stop 15_4
  if(size(dtp2_1,1) /= 2)                                   error stop 16_4
  do i=1,2
    if(any(dtp2_1(i)%i /= [-1,-2,-3]))                      error stop 17_4
  end do

  dtp3_1=dtp3%spread(spread(dtp3,1,2))
  if(dtp3_1%k1 /= 1 .or. dtp3_1%k2 /= 1)                     error stop 18_4
  if(dtp3_1%l1 /= 0 .or. dtp3_1%l2 /= 2)                     error stop 19_4
  if(size(dtp3_1,1) /= 2)                                    error stop 20_4
  do i=1,2
    if(any(dtp3_1(i)%i /= [4,5,6]))                          error stop 21_4
  end do


  dtp1_2=dtp1%spread(spread(spread(dtp1,1,2),1,3))

  if(dtp1_2%k1 /= 2 .or. dtp1_2%k2 /= 2)                     error stop 23_4
  if(dtp1_2%l1 /= 2 .or. dtp1_2%l2 /= 3)                     error stop 24_4
  do i=1,3
     if(any(dtp1_2(i,1)%i /= [1,2]))                         error stop 25_4
     if(any(dtp1_2(i,2)%i /= [1,2]))                         error stop 26_4
  end do

  dtp1_2=dtp1%spread(spread(spread(dtp1,1,2),2,3))

  if(dtp1_2%k1 /= 2 .or. dtp1_2%k2 /= 2)                     error stop 27_4
  if(dtp1_2%l1 /= 2 .or. dtp1_2%l2 /= 3)                     error stop 28_4
  do i=1,3
     if(any(dtp1_2(1,i)%i /= [1,2]))                         error stop 29_4
     if(any(dtp1_2(2,i)%i /= [1,2]))                         error stop 30_4
  end do

  dtp2_2=dtp2%spread(spread(spread(dtp2,1,2),1,3))

  if(dtp2_2%k1 /= 4 .or. dtp2_2%k2 /= 4)                     error stop 31_4
  if(dtp2_2%l1 /= -2 .or. dtp2_2%l2 /= 0)                    error stop 32_4
  do i=1,3
     if(any(dtp2_2(i,1)%i /= [-1,-2,-3]))                    error stop 33_4
     if(any(dtp2_2(i,2)%i /= [-1,-2,-3]))                    error stop 34_4
  end do

  dtp2_2=dtp2%spread(spread(spread(dtp2,1,2),2,3))

  if(dtp2_2%k1 /= 4 .or. dtp2_2%k2 /= 4)                     error stop 35_4
  if(dtp2_2%l1 /= -2 .or. dtp2_2%l2 /= 0)                    error stop 36_4
  do i=1,3
     if(any(dtp2_2(1,i)%i /= [-1,-2,-3]))                    error stop 37_4
     if(any(dtp2_2(2,i)%i /= [-1,-2,-3]))                    error stop 38_4
  end do

  dtp3_2=dtp3%spread(spread(spread(dtp3,1,2),1,3))

  if(dtp3_2%k1 /= 1 .or. dtp3_2%k2 /= 1)                     error stop 39_4
  if(dtp3_2%l1 /= 0 .or. dtp3_2%l2 /= 2)                     error stop 40_4
  do i=1,3
     if(any(dtp3_2(i,1)%i /= [4,5,6]))                       error stop 41_4
     if(any(dtp3_2(i,2)%i /= [4,5,6]))                       error stop 42_4
  end do

  dtp3_2=dtp3%spread(spread(spread(dtp3,1,2),2,3))

  if(dtp3_2%k1 /= 1 .or. dtp3_2%k2 /= 1)                     error stop 43_4
  if(dtp3_2%l1 /= 0 .or. dtp3_2%l2 /= 2)                     error stop 44_4
  do i=1,3
     if(any(dtp3_2(1,i)%i /= [4,5,6]))                       error stop 45_4
     if(any(dtp3_2(2,i)%i /= [4,5,6]))                       error stop 46_4
  end do

  dtp1_0=dtp1%spread(dtp1)

  if(dtp1_0%k1 /= 2 .or. dtp1_0%k2 /= 2)                    error stop 47_4
  if(dtp1_0%l1 /= 2 .or. dtp1_0%l2 /= 3)                    error stop 48_4
  if(any(dtp1_0%i /= [1,2] ))                               error stop 49_4

  dtp2_0=dtp2%spread(dtp2)

  if(dtp2_0%k1 /= 4 .or. dtp2_0%k2 /= 4)                    error stop 50_4
  if(dtp2_0%l1 /= -2 .or. dtp2_0%l2 /= 0)                   error stop 51_4
  if(any(dtp2_0%i /= [-1,-2,-3] ))                          error stop 52_4

  dtp3_0=dtp3%spread(dtp3)

  if(dtp3_0%k1 /= 1 .or. dtp3_0%k2 /= 1)                    error stop 53_4
  if(dtp3_0%l1 /= 0 .or. dtp3_0%l2 /= 2)                    error stop 54_4
  if(any(dtp3_0%i /= [4,5,6] ))                             error stop 55_4

end program
