!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssumedTypeParam04.f
!*
!*  DATE                       : July 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY
!* 3. ACTUAL ARGUMENT IS ALLOCATABLE AND POINTER
!* 4. USE EXTERNAL SUBROUTINE
!* 5. DEFECT 353958 353191
!234567890123456789012345678901234567890123456789012345678901234567890


module m
   type mytype(k1,k2,l1,l2)
      integer(2),kind :: k1
      integer(4),kind :: k2
      integer(2),len  :: l1
      integer(4),len  :: l2

      integer(k1) :: i1(l1:l2)
      integer(k2) :: i2(l2:l1+l2)
   end type
end module

  program dtParameterInquiryAssumedTypeParam04
  use m
  implicit none

  interface

    function getMytype(b)
      import
      type(mytype(k1=4,k2=8,l1=*,l2=*)),intent(in) :: b
      type(mytype(k1=4,k2=8,l1=b%l1,l2=b%l2))  :: getMytype
   end function

  end interface
  type(mytype(4,8,3,6)),target       :: t1
  type(mytype(k1=4,k2=8,l1=:,l2=:)),allocatable :: t2
  type(mytype(4,8,:,:)),pointer     :: t3
  type(mytype(4,8,:,:)),pointer     :: t4

  integer(t1%k1) :: i1=4
  integer(t1%k2) :: i2=5

  if(t1%k1 /= 4)                                            error stop 10_4
  if(t1%k2 /= 8)                                            error stop 11_4
  if(t1%l1 /= 3)                                            error stop 12_4
  if(t1%l2 /= 6)                                            error stop 13_4
  if(t1%k1%kind /= kind(t1%k1) .or. t1%k1%kind /= 2)        error stop 14_4
  if(t1%k2%kind /= kind(t1%k2) .or. t1%k2%kind /= 4)        error stop 15_4
  if(t1%l1%kind /= kind(t1%l1) .or. t1%l1%kind /= 2)        error stop 16_4
  if(t1%l2%kind /= kind(t1%l2) .or. t1%l2%kind /= 4)        error stop 17_4
  if(lbound(t1%i1,1) /=3 .or. ubound(t1%i1,1) /=6)  error stop 18_4
  if(lbound(t1%i2,1) /=6 .or. ubound(t1%i2,1) /=9)          error stop 19_4

  allocate(mytype(4,8,3,6) :: t2)

  if(t2%k1 /= 4)                                            error stop 20_4
  if(t2%k2 /= 8)                                            error stop 21_4
  if(t2%l1 /= 3)                                           error stop 22_4
  if(t2%l2 /= 6)                                           error stop 23_4
  if(t2%k1%kind /= kind(t2%k1) .or. t2%k1%kind /= 2)        error stop 24_4
  if(t2%k2%kind /= kind(t2%k2) .or. t2%k2%kind /= 4)        error stop 25_4

  if(t2%l1%kind /= kind(t2%l1) .or. t2%l1%kind /= 2)        error stop 26_4
  if(t2%l2%kind /= kind(t2%l2) .or. t2%l2%kind /= 4)        error stop 27_4
  if(lbound(t2%i1,1) /=3 .or. ubound(t2%i1,1) /=6)          error stop 28_4
  if(lbound(t2%i2,1) /=6 .or. ubound(t2%i2,1) /=9)        error stop 29_4


  t3=>t1

  if(t3%k1 /= 4)                                            error stop 30_4
  if(t3%k2 /= 8)                                            error stop 31_4
  if(t3%l1 /= 3)                                            error stop 32_4
  if(t3%l2 /= 6)                                            error stop 33_4
  if(t3%k1%kind /= kind(t3%k1) .or. t3%k1%kind /= 2)        error stop 34_4
  if(t3%k2%kind /= kind(t3%k2) .or. t3%k2%kind /= 4)        error stop 35_4
  if(t3%l1%kind /= kind(t3%l1) .or. t3%l1%kind /= 2)        error stop 36_4
  if(t3%l2%kind /= kind(t3%l2) .or. t3%l2%kind /= 4)        error stop 37_4
  if(lbound(t3%i1,1) /=t3%l1 .or. ubound(t3%i1,1) /=t3%l2)  error stop 38_4
  if(lbound(t3%i2,1) /=6 .or. ubound(t3%i2,1) /=9)          error stop 39_4

  allocate(t4,source=getMytype(t3))

  if(t4%k1 /= 4)                                            error stop 40_4
  if(t4%k2 /= 8)                                            error stop 41_4
  if(t4%l1 /= 3)                                            error stop 42_4
  if(t4%l2 /= 6)                                            error stop 43_4
  if(t4%k1%kind /= kind(t4%k1) .or. t4%k1%kind /= 2)        error stop 44_4
  if(t4%k2%kind /= kind(t4%k2) .or. t4%k2%kind /= 4)        error stop 45_4
  if(t4%l1%kind /= kind(t4%l1) .or. t4%l1%kind /= 2)        error stop 46_4
  if(t4%l2%kind /= kind(t4%l2) .or. t4%l2%kind /= 4)        error stop 47_4
  if(lbound(t4%i1,1) /=t4%l1 .or. ubound(t4%i1,1) /=t4%l2)  error stop 48_4
  if(lbound(t4%i2,1) /=6 .or. ubound(t4%i2,1) /=9)          error stop 49_4


end

    function getMytype(b)
      use m,only: mytype
      type(mytype(k1=4,k2=8,l1=*,l2=*)),intent(in) :: b
      type(mytype(k1=4,k2=8,l1=b%l1,l2=b%l2))  :: getMytype
      getMytype = b
   end function

