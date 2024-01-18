!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryArrayIntegerComp06.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 15 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY FOR DT AND COMPONENT
!* 3. DIFFERENT TYPE PARAMETER
!* 4. INTEGER POINTER ARRAY COMPONENT
!* 5. DEFECT 353790,352295,353357
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
     integer(1),kind  :: k1=2
     integer(k1),kind :: k2=2
     integer(2),len   :: l1=4
     integer(k1+k2),len :: l2=1
     
     integer(1),dimension(:,:),pointer :: i1=>null()
     integer(2),pointer :: i2(:)=>null()
     integer(k2),pointer :: i3(:)=>null()
     integer(8),pointer :: i4(:,:)=>null()
     integer(k1+k2),dimension(:),pointer :: i5=>null() 
     integer(k1),pointer :: i6(:)=>null()
     integer(k2),pointer :: i7(:)=>null()
     integer(1),pointer :: i8(:)=>null()
     integer(k1),pointer :: i9(:)=>null()
     integer(4),dimension(:),pointer :: i10=>null()
     integer(2+2),pointer :: i11(:)=>null()
     integer(2),pointer :: i12(:)=>null()
     integer(max(2,4)),pointer :: i13(:,:)=>null()

   end type 
         
end module

  program dtParameterInquiryArrayIntegerComp06
  use m
  implicit none

  type(base) :: t

  integer(t%i3%kind),target :: a3(t%k1%kind:5)
  integer(t%k1+t%k2),target :: a5(t%k1:t%k2)
  integer(t%k1),target :: a9(3:4)

  allocate(t%i1(3,3))
  allocate(integer(2) :: t%i2(3:-4)) 
  t%i3=>a3
  allocate(integer(8) :: t%i4(-1:t%k1,(t%k2+3):7))
  t%i5=>a5 
  allocate(t%i6(2))

  t%i7=>t%i3
  allocate(t%i8(t%k2%kind) )
  t%i9=>a9
  allocate(integer(2*2) :: t%i10(kind(t%k1): t%k1%kind))

  allocate(integer(4) :: t%i11(t%k2))
  allocate(t%i12(t%k1+t%k2))
  allocate(integer(4) :: t%i13(ubound(t%i4,1),ubound(t%i4,2)))

  

  if(t%k1 /= 2)                                     error stop 10_4
  if(t%k2 /= 2)                                     error stop 11_4 
  if(t%l1 /= 4)                                     error stop 12_4
  if(t%l2 /= 1)                                     error stop 13_4

  if(t%k1%kind /=kind(t%k1) .or. t%k1%kind /= 1)    error stop 14_4
  if(t%k2%kind /=kind(t%k2) .or. t%k2%kind /= 2)    error stop 15_4
  if(t%l1%kind /=kind(t%l1) .or. t%l1%kind /= 2)    error stop 16_4
  if(t%l2%kind /=kind(t%l2) .or. t%l2%kind /= 4)    error stop 17_4

  if(t%i1%kind /=kind(t%i1) .or. t%i1%kind /= 1)    error stop 18_4
  if(t%i2%kind /=kind(t%i2) .or. t%i2%kind /= 2)    error stop 19_4
  if(t%i3%kind /=kind(t%i3) .or. t%i3%kind /= 2)    error stop 20_4
  if(t%i4%kind /=kind(t%i4) .or. t%i4%kind /= 8)    error stop 21_4

  if(t%i5%kind /=kind(t%i5) .or. t%i5%kind /= 4)    error stop 22_4
  if(t%i6%kind /=kind(t%i6) .or. t%i6%kind /= 2)    error stop 23_4
  if(t%i7%kind /=kind(t%i7) .or. t%i7%kind /= 2)    error stop 24_4
  if(t%i8%kind /=kind(t%i8) .or. t%i8%kind /= 1)    error stop 25_4
  if(t%i9%kind /=kind(t%i9) .or. t%i9%kind /= 2)    error stop 26_4
  if(t%i10%kind /=kind(t%i10) .or. t%i10%kind /= 4)    error stop 27_4
  if(t%i11%kind /=kind(t%i11) .or. t%i11%kind /= 4)    error stop 28_4
  if(t%i12%kind /=kind(t%i12) .or. t%i12%kind /= 2)    error stop 29_4
  if(t%i13%kind /=kind(t%i13) .or. t%i13%kind /= 4)    error stop 30_4  

  if(ubound(t%i1,1) /=3 .or. lbound(t%i1,1) /= 1)      error stop 31_4
  if(ubound(t%i1,2) /=3 .or. lbound(t%i1,2) /= 1)      error stop 32_4
  if(ubound(t%i2,1) /=0 .or. lbound(t%i2,1) /= 1)      error stop 33_4
  
  if(ubound(t%i3,1) /=5 .or. lbound(t%i3,1) /= 1)      error stop 34_4  
  if(ubound(t%i4,1) /=2 .or. lbound(t%i4,1) /= -1)     error stop 35_4
  if(ubound(t%i4,2) /=7 .or. lbound(t%i4,2) /= 5)      error stop 36_4

  if(ubound(t%i5,1) /=2 .or. lbound(t%i5,1) /= 2)      error stop 37_4
  if(ubound(t%i6,1) /=2 .or. lbound(t%i6,1) /= 1)      error stop 38_4
  if(ubound(t%i7,1) /=5 .or. lbound(t%i7,1) /= 1)      error stop 39_4

  if(ubound(t%i8,1) /=2 .or. lbound(t%i8,1) /= 1)      error stop 40_4
  if(ubound(t%i9,1) /=4 .or. lbound(t%i9,1) /= 3)      error stop 41_4
  if(ubound(t%i10,1) /=1 .or. lbound(t%i10,1) /= 1)    error stop 42_4
  if(ubound(t%i11,1) /=2 .or. lbound(t%i11,1) /= 1)    error stop 43_4
  if(ubound(t%i12,1) /=4 .or. lbound(t%i12,1) /= 1)    error stop 44_4
  if(ubound(t%i13,1) /=2 .or. lbound(t%i13,1) /= 1)    error stop 45_4
  if(ubound(t%i13,2) /=7 .or. lbound(t%i13,2) /= 1)    error stop 46_4

  end
