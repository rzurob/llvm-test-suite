!*********************************************************************
!*  ===================================================================
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
!* 3. ARRAY SECTION
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(len=3)         :: c1(3)="abc"
   contains
      function getchar(c1)
         character(*),intent(in) :: c1(:)
         character(c1%len):: getchar(size(c1))
         getchar=c1
      end function
end module

program typeParamInquiryIntrinsicArrSect01_d354698
    use m
    implicit none
    character(:),allocatable :: c2(:)
    allocate( c2(len(getchar(c1))),source=getchar(c1))
    if( c2(1:1)%len /= len(c2(1:1)) .or. c2(1:1)%len /= 3)   error stop 10_4
end

