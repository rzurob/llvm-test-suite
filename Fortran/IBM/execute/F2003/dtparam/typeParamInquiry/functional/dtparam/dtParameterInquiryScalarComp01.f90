!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 10 2008 
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
!* 3. DIFFERENT TYPE PARAMETER
!* 4. INTEGER SCALAR COMPONENT 
!* 5. TYPE PARAMETER VALUE ARE NEGATIVE
!* 6. DEFECT 353194
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type :: base(k1,k2,l1,l2)
        integer,kind :: k1
        integer(4),kind :: k2
        integer(k2),len :: l1
        integer(2*2),len :: l2 
        
        integer(8)      :: i1=-k1 
        integer(k2)     :: i2=k1+k2
        integer(selected_int_kind(k1+k2)) :: i3
   end type

end module

  program dtParameterInquiryScalarComp01 
  use m
  implicit none

  
  type(base(8,2,-2,1)) :: t 
 
  if(t%k1 /= 8 .or. t%k2 /= 2 )                     error stop 10_4
  if(t%l1 /= -2 .or. t%l2 /= 1 )                     error stop 11_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 4)    error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 4)    error stop 13_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 2)    error stop 14_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 4)    error stop 15_4
   
  if(t%i1 /= -8)                                     error stop 16_4
  if(t%i1%kind /= kind(t%i1) .or. t%i1%kind /=8)     error stop 17_4 
  if(t%i2 /= 10)                                     error stop 18_4
  if(t%i2%kind /= kind(t%i2) .or. t%i2%kind /=2)     error stop 19_4 
  if(t%i3%kind /= kind(t%i3) .or. t%i3%kind /=selected_int_kind(10))     error stop 20_4  

  end
