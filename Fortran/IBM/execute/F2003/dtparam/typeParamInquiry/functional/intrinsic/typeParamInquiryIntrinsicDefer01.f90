!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : June 27 2008 
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
!* 4. ALLOCATABLE AND POINTER SCALAR 
!* 5. USE ASSIGNMENT
!* 6. DEFECT 352957
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   character(:),allocatable :: a2 
   character(:),pointer :: p2=>null()
end module 
program typeParamInquiryIntrinsicDefer01
    use m
    implicit none
    
    character(:),allocatable :: a1
    character(:),pointer :: p1=>null()
    character(len=*),parameter :: c1='xlftest'
    character(len=-1),target :: c2=''
    character(len=7),target  :: c3="hello"

    a1=c1
    if( a1%len /= c1%len .or. a1%len /= len(a1)   .or. &
        a1%len /= 7 )                                   error stop 5_4

    if(a1 /= "xlftest")                                 error stop 6_4

    a1=c1(4:7)
    if( a1%len /=len(a1) .or. a1%len /= 4)              error stop 7_4
    if(a1 /= "test")                                    error stop 8_4

    a1=c2
    if( a1%len /= c2%len .or. a1%len /= len(a1)   .or. &
        a1%len /= 0 )                                  error stop 9_4
    if(a1 /= '')                                        error stop 10_4

    a2="Hi"
    if(a2%len /= len(a2) .or. a2%len /= 2)              error stop 11_4
  
    a2="Hi,Hi,Hi"(4:5)
    if(a2%len /= len(a2) .or. a2%len /= 2)              error stop 12_4
    if(a2 /= "Hi")                                      error stop 13_4
 
    a2(:)=''
    a1=a2
    if(a1%len /= len(a1) .or. a1%len /= 2)              error stop 14_4
    if(a2%len /= len(a2) .or. a2%len /= 2)              error stop 15_4
   
    a2=''
    if(a2%len /= len(a2) .or. a2%len /= 0 )             error stop 16_4
    
    p1=>c3
    p2=>p1(2:4)
    if(p1%len /= len(p1) .or. p1%len /= 7)              error stop 17_4
    if(p2%len /= len(p2) .or. p2%len /= 3)              error stop 18_4 
    if(p1 /= 'hello')                                   error stop 19_4
    if(p2 /= 'ell')                                     error stop 20_4

    p1=>c2
    p2=>p2(2:3)
    if(p1%len /= len(p1) .or. p1%len /= 0)              error stop 21_4
    if(p2%len /= len(p2) .or. p2%len /= 2)              error stop 22_4
    if(p1 /= '')                                        error stop 23_4
    if(p2 /= 'll')                                      error stop 24_4

end

