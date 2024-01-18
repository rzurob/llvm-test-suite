!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryScalarComp03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 11 2008 
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
!* 4. SCALAR CHARACTER COMPONENT 
!* 5. DEFECT 353331
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type base(k1,k2,l1,l2)
       integer(2),kind    :: k1
       integer(2*k1),kind :: k2
       
       integer(k1),len    :: l1
       integer(2**1),len  :: l2

       character(kind=k1,len=k2)       :: c1="xlftest" 
       character(len=l1+l2)            :: c2="fortran" !-- defect 353331--!
       character(-10)                  :: c3=''
       character(len=c1%len)           :: c4="xlf"
       character(len=l1*l2)            :: c5="good morning"
       character(l1)                   :: c6="hello"
       character                       :: c7=char(k1)       
   end type
end module

  program dtParameterInquiryScalarComp03
  use m
  implicit none

  type(base(1,8,4,8)) :: t

  if(t%k1 /= 1)                                             error stop 10_4
  if(t%k1%kind /= kind(t%k1) .or. t%k1%kind /= 2)           error stop 11_4

  if(t%k2 /= 8)                                             error stop 12_4
  if(t%k2%kind /= kind(t%k2) .or. t%k2%kind /= 2)           error stop 13_4

  if(t%l1 /= 4)                                             error stop 14_4
  if(t%l1%kind /= kind(t%l1) .or. t%l1%kind /= 1)           error stop 15_4

  if(t%l2 /= 8)                                             error stop 16_4
  if(t%l2%kind /= kind(t%l2) .or. t%l2%kind /= 2)           error stop 17_4

  if(t%c1 /= 'xlftest')                                     error stop 18_4
  if(t%c1%kind /= kind(t%c1) .or. t%c1%kind /= 1)           error stop 19_4
  if(t%c1%len  /= len(t%c1)  .or.  t%c1%len /= 8)           error stop 20_4

  if(t%c2 /= 'fortran')                                     error stop 21_4
  if(t%c2%kind /= kind(t%c2) .or. t%c2%kind /= 1)           error stop 22_4
  if(t%c2%len  /= len(t%c2)  .or. t%c2%len  /= 12)          error stop 23_4

  if(t%c3 /= '')                                            error stop 24_4
  if(t%c3%kind /= kind(t%c3) .or. t%c3%kind /= 1)           error stop 25_4
  if(t%c3%len  /= len(t%c3)  .or. t%c3%len  /= 0)           error stop 26_4

  if(t%c4 /= 'xlf')                                         error stop 27_4
  if(t%c4%kind /= kind(t%c4) .or. t%c4%kind /= 1)           error stop 28_4
  if(t%c4%len  /= len(t%c4)  .or. t%c4%len  /= 8)           error stop 29_4

  if(t%c5 /= 'good morning')                                error stop 30_4
  if(t%c5%kind /= kind(t%c5) .or. t%c5%kind /= 1)           error stop 31_4
  if(t%c5%len  /= len(t%c5)  .or. t%c5%len  /= 32)          error stop 32_4

  if(t%c6 /= 'hell')                                        error stop 33_4
  if(t%c6%kind /= kind(t%c6) .or. t%c6%kind /= 1)           error stop 34_4
  if(t%c6%len  /= len(t%c6)  .or. t%c6%len  /= 4)           error stop 35_4

   
  end
