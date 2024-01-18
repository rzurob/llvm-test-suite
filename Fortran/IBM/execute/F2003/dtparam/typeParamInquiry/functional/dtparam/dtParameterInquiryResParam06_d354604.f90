!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryResParam06_d354604.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 2 2008 
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
!* 3. DEFECT 354604
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base(l)
       integer,len :: l
    end type
    contains
    pure integer function getlen3(a)
       type(base(*)),intent(in) :: a
          getlen3=a%l
    end function

end module

program dtParameterInquiryResParam06_d354604
  use m

  implicit none
  call test()
end

subroutine test()
use m
  type(base(3)),parameter :: t6=base(3)()
  character(len=getlen3(t6)) :: c(getlen3(t6))
  type(base(getlen3(t6))) :: t7(getlen3(t6))
end subroutine
