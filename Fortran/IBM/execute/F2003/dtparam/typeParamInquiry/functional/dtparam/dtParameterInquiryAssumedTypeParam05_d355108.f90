!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssumedTypeParam05_d355108.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 15 2008 
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
!* 3. DEFECT 355108
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer(4),len :: l
      character(l)  :: c1  
   end type

end module

  program dtParameterInquiryAssumedTypeParam05_d355108 
  use m
  implicit none
  
  type(base(l=:)),allocatable:: b2

  b2=get()
  if(b2%l%kind /= kind(b2%l) .or. b2%l%kind /= 4)  error stop 10_4 
  if(b2%c1 /= "xlftest")                           error stop 11_4
  if(b2%l /= 7)                                    error stop 12_4
  if(b2%c1%len /= len(b2%c1) .or. b2%c1%len /= 7)  error stop 13_4
  contains
    function get()
       type(base(:)),allocatable :: get
       allocate(get,source=base(l=7)(c1="xlftest"))
    end function

end
