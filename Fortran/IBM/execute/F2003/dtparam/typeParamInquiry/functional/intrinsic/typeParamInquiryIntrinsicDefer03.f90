!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 8 2008 
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
!* 3. DEFERRED TYPE PARAMETER INQUIRY
!* 4. ALLOCATABLE AND POINTER ARRAY 
!* 5. USE ASSIGNMENT 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    character(:),allocatable :: c2(:,:) 
    character(:),dimension(:,:),pointer :: p2 => null()
end module 
program typeParamInquiryIntrinsicDefer03
    use m
    implicit none

    character(:),allocatable :: c1(:,:)
    character(7) :: c3(2,2)=  &
                reshape((/ character(7) :: 'this','is ','a','test'/),(/2,2/))
    character(:),pointer :: p1(:,:)
    character(4),target :: t1(6,3)

    c1=c3
    if(c1%len /= len(c1) .or. c1%len /=7 .or. len(c1) /=7)    error stop 10_4
    if(c1%kind /=kind(c1) .or. c1%kind /=1 .or. kind(c1) /=1) error stop 11_4 

    c1=c3(1:2,2:2)    
    if(c1%len /= len(c1) .or. c1%len /=7 .or. len(c1) /=7)    error stop 12_4
    if(c1%kind /=kind(c1) .or. c1%kind /=1 .or. kind(c1) /=1) error stop 13_4   

    c1="fortran xlftest"
    if(c1%len /= len(c1) .or. c1%len /=15 .or. len(c1) /=15)    error stop 14_4
    if(c1%kind /=kind(c1) .or. c1%kind /=1 .or. kind(c1) /=1) error stop 15_4

    deallocate(c1)
    p1=> t1 
    c1=p1 
    if(c1%len /= p1%len .or. c1%len /= 4 .or. len(c1) /=4)    error stop 16_4
    if(c1%kind /= p1%kind .or. c1%kind /= 1  &
                              .or. kind(c1) /= 1)             error stop 17_4    
    p2=>p1
    if(p2%len /= len(p2) .or. p2%len /= 4)                    error stop 18_4
  
    p2=>p1(1:1,2:2)
    if(p2%len /= len(p2) .or. p2%len /= 4)                    error stop 19_4

    p2(2:,3:)=>p1(:,2:)
    if(p2%len /= len(p2) .or. p2%len /= 4)                    error stop 20_4
    if(p2%kind /=kind(p2) .or. p2%kind /= 1)                  error stop 21_4

end

