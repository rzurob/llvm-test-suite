!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Auguest 3 2008 
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
!* 5. USE ALLOCATE 
!* 6. DEFECT 354585
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   character(:),allocatable :: a2
   character(:),pointer     :: p2 =>null() 
end module 
program typeParamInquiryIntrinsicDefer02
    use m
    implicit none
    
    character(:),allocatable :: a1
    character(:),allocatable :: c1
    character(len=*),parameter :: c2="xlftest"
    character(2)    :: c3
    character(:),pointer :: c4
    character(:),allocatable :: c5  
    character(:),pointer :: p1=>null()
    character(20),target   :: t1='good morning'
    integer :: i=4

    if(allocated(c1))  deallocate(c1)
    allocate(character(len=10) :: c1)
    a1=c1
    if(a1%len /= len(a1) .or. a1%len /= 10)                error stop 10_4 
    if(c1%len /= len(c1) .or. c1%len /= 10)                error stop 11_4
    deallocate(c1)

    if(allocated(a2))  deallocate(a2)
    allocate(character(c2%len) :: a2)
    if(a2%len /=len(a2) .or. a2%len /=7)                   error stop 12_4

    if(allocated(a2))  deallocate(a2)
    allocate(character(len(1_"xlftest"(4:7))) :: a2) 
    if(a2%len /= len(a2) .or. a2%len /= 4)                 error stop 13_4

    a1=a2(1:3)
    if(a1%len /= len(a1) .or. a1%len /=3)                  error stop 14_4
    deallocate(a2)
   
    if(allocated(a1))  deallocate(a1) 
    allocate(a1,source=c3)
    if( a1%len /= len(a1) .or. a1%len /= 2)                error stop 15_4
   
    if(allocated(a2))  deallocate(a2) 
    allocate(a2,source=c2(4:))
    if(a2%len /= len(a2) .or. a2%len /= 4)                 error stop 16_4
    deallocate(a2)
   
    allocate(c4,source=1_"Fortran ")
    allocate(a2,source="Test Team")
    c5=c4//a2
    if(c5%len /= len(c5) .or. c5%len /= (c4%len+a2%len))   error stop 17_4
    deallocate(a2,c4,c5)
  
    allocate(p1,source="hello") 
    p2=>p1
    if( p1%len /=len(p1) .or. p1%len /= 5)                 error stop 18_4
    if( p2%len /=len(p2) .or. p2%len /= 5)                 error stop 19_4
    if( p2 /= 'hello')                                     error stop 20_4 

    allocate(p1,source=t1(1:4))
    if(p1%len /= len(p1) .or. p1%len /= 4)                 error stop 21_4 
    if( p1 /= 'good')                                      error stop 22_4

    allocate(p2,source=p1(1:2)) 
    if( p2%len  /=2   .or. len(p2)  /= 2)                  error stop 23_4
    if( p2%kind /=1   .or. kind(p2) /= 1)                  error stop 24_4
    if( p2 /= 'go')                                        error stop 25_4

    allocate(p2,source=p1//' '//p2//p1(3:4)) 
    if( p2%len  /=9   .or. len(p2)  /= 9)                  error stop 26_4
    if( p2%kind /=1   .or. kind(p2) /= 1)                  error stop 27_4
    if( p2 /= 'good good')                                 error stop 28_4

    deallocate(p2)
    allocate(character(len=5) :: p2)
    if(p2%len /=5 .or. len(p2) /= 5)                       error stop 29_4

    deallocate(p2)
    allocate(character(c3%len+len(c3)+i) :: p2)                       
    if(p2%len /= len(p2) .or. p2%len /= 8)                 error stop 30_4 

    deallocate(p2)
    allocate(character(c2%len+c2%len+i) :: p2)           
    if(p2%len /= len(p2) .or. p2%len /= 18)                error stop 31_4            
    if(allocated(a1)) deallocate(a1)
    allocate(character(len=5) :: a1)
    if(a1%len /=len(a1) .or. len(a1) /= 5)                 error stop 32_4

    if(allocated(a1)) deallocate(a1)
    allocate(character(c3%len+len(c3)+i) :: a1)
    if(a1%len /= len(a1) .or. a1%len /= 8)                 error stop 33_4


    if(allocated(a1)) deallocate(a1)
    allocate(character(c2%len+c2%len+i) :: a1)
    if(a1%len /= len(a1) .or. a1%len /= 18)                error stop 34_4

end

