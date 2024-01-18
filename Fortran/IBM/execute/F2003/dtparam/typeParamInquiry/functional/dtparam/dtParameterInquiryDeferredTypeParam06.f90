!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDefferredTypeParam06.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 24 2008 
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. WITH CHARACTER COMPONENT
!* 4. DERIVED TYPE IS ARRAY
!* 5. DEFECT 354013
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer(2),len :: l
      character(len=l) :: c(l-1:l+1)
   end type 
end module

  program dtParameterInquiryDeferredTypeParam06 
  use m
  implicit none

  type(base(5)),target :: b1(4) 
  type(base(:)),allocatable :: b2(:)
  type(base(:)),pointer ::b3(:,:) =>null()
  class(base(:)),pointer :: b4(:) =>null()
  type(base(:)),allocatable :: b5(:,:)
  integer :: i

  allocate(base(l=b1%l) :: b2(2*b1%l))
  if(b2%l /= 5)                                               error stop 10_4
  if(ubound(b2,1) /=10 .or. lbound(b2,1) /= 1)                error stop 11_4
  do i=1,ubound(b2,1)
     if(b2(i)%c%len /= len(b2(i)%c) .or. b2(i)%c%len /= 5)    error stop 12_4
  if(ubound(b2(i)%c,1) /=6 .or. lbound(b2(i)%c,1) /= 4)       error stop 13_4 
  end do

  do i=1,10
    allocate(base(i) :: b3(i,i+1))
    print *,b3(i,i+1)%c%len,len(b3(i,i+1)%c),b3(i,i+1)%c%len /= i
    if(b3(i,i+1)%l /=i)                                       error stop 14_4
    if(b3(i,i+1)%c%len /= len(b3(i,i+1)%c)                &
           .or. b3(i,i+1)%c%len /= i)                         error stop 15_4
    if(lbound(b3(i,i+1)%c,1) /=i-1  & 
                .or. ubound(b3(i,i+1)%c,1) /=i+1)             error stop 16_4

  end do

  b4=>b1

  if(b4%l /= 5)                                               error stop 17_4
  if(ubound(b4,1) /=4 .or. lbound(b4,1) /= 1)                 error stop 18_4
  do i=1,ubound(b4,1)
     if(b4(i)%c%len /= len(b4(i)%c) .or. b4(i)%c%len /= 5)    error stop 19_4
  if(ubound(b4(i)%c,1) /=6 .or. lbound(b4(i)%c,1) /= 4)       error stop 20_4
  end do  


  b5=b3

  do i=1,10
    print *,b5(i,i+1)%l,i
    if(b5(i,i+1)%l /=i)                                       error stop 21_4
    if(b5(i,i+1)%c%len /= len(b5(i,i+1)%c)                &
           .or. b5(i,i+1)%c%len /= i)                         error stop 22_4
    if(lbound(b5(i,i+1)%c,1) /=i-1  &
                .or. ubound(b5(i,i+1)%c,1) /=i+1)             error stop 23_4

  end do
end
