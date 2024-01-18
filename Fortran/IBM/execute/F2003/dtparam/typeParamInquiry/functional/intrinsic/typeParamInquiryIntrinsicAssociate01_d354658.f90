!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicAssociate01_d354658.f
!*
!*  DATE                       : August 6 2008
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. USE ASSOCIATE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(len=3)         :: c1(1)="abc"
   contains
      function getchar(c1)
         character(*),intent(in) :: c1(:)
         character(c1%len):: getchar(size(c1))
         getchar=c1
      end function
end module

program typeParamInquiryIntrinsicAssociate01_d354658
    use m
    implicit none
      associate(x=>getchar(c1))
         if(x%len /= len(x) .or. x%len /= 3)            error stop 10_4
         if(ubound(x,1) /= 1)                           error stop 11_4
     end associate
end

