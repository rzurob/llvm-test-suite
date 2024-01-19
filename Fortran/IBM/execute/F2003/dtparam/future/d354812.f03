!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 8 2008
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
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicAssociate05_354812
    implicit none

    character(:),pointer :: p2(:)
    allocate(p2(3),source=["ab","cd","ef"])
    print *,p2(1)(2:2),len(p2(1)(2:2)),p2(1)(2:2)%len
end
