!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryDTIntrinsic02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 6 2008 
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. COMPONENT IS POINTER 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type t
      integer(kind=2), pointer :: i1
      integer,pointer          :: i2(:)
      
      logical(2),pointer   :: l1
      logical, pointer     :: l2(:)
      
      real(8),pointer      :: r1
      real(16),pointer     :: r2
      
      complex,pointer      :: x1
      complex(8),pointer   :: x2

      character(:),pointer :: c1
      character(:),pointer :: c2(:)
   end type

end module

  program typeParamInquiryDTIntrinsic02
  use m
  implicit none

  character(len=len("xlf")),target :: c2
  integer :: state
  character(256) :: msg
  type(t)  :: tt

  if(tt%i1%kind /= kind(tt%i1) .or. tt%i1%kind /= 2) error stop 10_4
  if(tt%i2%kind /= kind(tt%i2) .or. tt%i2%kind /= 4) error stop 11_4

  if(tt%l1%kind /= kind(tt%l1) .or. tt%l1%kind /= 2) error stop 12_4
  if(tt%l2%kind /= kind(tt%l2) .or. tt%l2%kind /= 4) error stop 13_4

  if(tt%r1%kind /= kind(tt%r1) .or. tt%r1%kind /= 8) error stop 14_4
  if(tt%r2%kind /= kind(tt%r2) .or. tt%r2%kind /= 16) error stop 15_4

  if(tt%x1%kind /= kind(tt%x1) .or. tt%x1%kind /= 4) error stop 16_4
  if(tt%x2%kind /= kind(tt%x2) .or. tt%x2%kind /= 8) error stop 17_4

  if(tt%c1%kind /= kind(tt%c1) .or. tt%c1%kind /= 1)  error stop 18_4
  if(tt%c2%kind /= kind(tt%c2) .or. tt%c2%kind /= 1)  error stop 19_4

  allocate(tt%c1,source=c2,stat=state,errmsg=msg) 
  if(state /= 0) then
    print *,msg
    return
  end if
  if(tt%c1%len /= len(tt%c1) .or. tt%c1%len /= 3)  error stop 20_4
  allocate(tt%c2(1:2),source=['abcd','efgh'],stat=state,errmsg=msg)
  if(state /= 0) then
     print *,msg
     return
  endif
  if(tt%c2%len /= len(tt%c2) .or. tt%c2%len /= 4)  error stop 21_4 

end
