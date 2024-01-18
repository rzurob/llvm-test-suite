!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicAssociate03_d354781.f
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 7 2008  
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
!* 3. TYPE PARAMETER INQUIRY INSIDE ASSOCIATE 
!* 4. SELECTOR IS CHARACTER EXPRESSION
!234567890123456789012345678901234567890123456789012345678901234567890
program typeParamInquiryIntrinsicAssociate03_d354781

    implicit none

    character(len=*),parameter :: c3(3)=['abc','def','ghi']
    character(:),allocatable :: c5(:)

    allocate(c5(1),source=c3(1:1))
    print *,c5(1)(1:1)
    associate(x1=>c5(1)(1:1)) 
    end associate

end
