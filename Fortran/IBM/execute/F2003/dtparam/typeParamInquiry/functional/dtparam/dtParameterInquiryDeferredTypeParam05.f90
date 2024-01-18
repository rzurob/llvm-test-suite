!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDefferredTypeParam05.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 23 2008 
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
!* 3. WITHOUT COMPONENT
!* 4. DUMMY ARGUMENT IS INTENT(IN) ALLOCATABLE OR POINTER DERIVED TYPE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
   end type
   
   contains

     subroutine checkTP1(b1)
        type(base(4,:)),allocatable,intent(in) :: b1
        if(b1%k /= 4)                                      error stop 10_4
        if(b1%l /= 10)                                     error stop 11_4
        if(b1%k%kind /=kind(b1%k) .or. b1%k%kind /= 2)     error stop 12_4
        if(b1%l%kind /=kind(b1%l) .or. b1%l%kind /= 8)     error stop 13_4
     end subroutine

     subroutine checkTP2(b2)
        type(base(2,:)),pointer,intent(in) :: b2
        if(b2%k /= 2)                                      error stop 14_4
        if(b2%k%kind /=kind(b2%k) .or. b2%k%kind /= 2)     error stop 15_4
        if(.not. associated(b2)) then
          print *,"b2 is not associated"
        else
          print *,"b2 is associated"
          if(b2%l /= 3)                                    error stop 16_4
          if(b2%l%kind /=kind(b2%l) .or. b2%l%kind /= 8)   error stop 17_4
        endif
     end subroutine

     subroutine setTP(b)
        type(base(2,:)),pointer,intent(out) :: b
        allocate(base(2,5) :: b)
     end subroutine
end module

  program dtParameterInquiryDeferredTypeParam05 
  use m
  implicit none
  
  type(base(4,:)),allocatable :: b1
  type(base(2,:)),pointer  :: b2=>null()

  allocate(base(4,10) :: b1)
  call checkTP1(b1)
  call checkTP2(b2)
  allocate(base(2,3) :: b2)
  call checkTP2(b2)
  call setTP(b2)

  if(b2%k /= 2)                                              error stop 18_4
  if(b2%l /= 5)                                              error stop 19_4 
  if(b2%k%kind /=kind(b2%k) .or. b2%k%kind /= 2)             error stop 20_4
  if(b2%l%kind /=kind(b2%l) .or. b2%l%kind /= 8)             error stop 21_4
 
end
