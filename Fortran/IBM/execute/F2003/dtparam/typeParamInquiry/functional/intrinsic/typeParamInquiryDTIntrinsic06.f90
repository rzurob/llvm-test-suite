!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryDTIntrinsic06.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 7 2008 
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
!* 3. INQUIRY THROUGH TYPE-BOUND PROCEDURE 
!* 4. COMPONENT IS PRIVATE
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base
     private
     integer :: i1
     integer(2) :: i2(3)
     complex :: x1
     complex(8) :: x2(3)
     logical :: l1
     logical(1) :: l2(2)
     character(len=3) :: c1
     character(len=2) :: c2(3) 

     character(:),allocatable :: c3
     character(:),allocatable :: c4(:)

     character(:),pointer  :: c5
     character(:),pointer  :: c6(:)

     contains

         procedure,pass :: getDT => sub
    
   end type

     contains
     subroutine sub(dt)
        class(base) :: dt

        allocate(dt%c3,source="hello")
        allocate(character(3):: dt%c4(2:5))
        allocate(dt%c5,source="xlftest")
        allocate(dt%c6(1:3),source=["ab","cd","ef"])

        if(dt%i1%kind /= kind(dt%i1) .or. dt%i1%kind /= 4) error stop 10_4
        if(dt%i2%kind /= kind(dt%i2) .or. dt%i2%kind /= 2) error stop 11_4
        if(dt%x1%kind /= kind(dt%x1) .or. dt%x1%kind /= 4) error stop 12_4
        if(dt%x2%kind /= kind(dt%x2) .or. dt%x2%kind /= 8) error stop 13_4
        if(dt%c1%kind /= kind(dt%c1) .or. dt%c1%kind /= 1) error stop 14_4
        if(dt%c2%kind /= kind(dt%c2) .or. dt%c2%kind /= 1) error stop 15_4
        if(dt%c3%kind /= kind(dt%c3) .or. dt%c3%kind /= 1) error stop 16_4
        if(dt%c4%kind /= kind(dt%c4) .or. dt%c4%kind /= 1) error stop 17_4
        if(dt%c5%kind /= kind(dt%c5) .or. dt%c5%kind /= 1) error stop 18_4
        if(dt%c6%kind /= kind(dt%c6) .or. dt%c6%kind /= 1) error stop 19_4

        if(dt%c1%len  /= len(dt%c1)  .or. dt%c1%len  /= 3) error stop 20_4
        if(dt%c2%len  /= len(dt%c2)  .or. dt%c2%len  /= 2) error stop 21_4
        if(dt%c3%len  /= len(dt%c3)  .or. dt%c3%len  /= 5) error stop 22_4
        if(dt%c4%len  /= len(dt%c4)  .or. dt%c4%len  /= 3) error stop 23_4
        if(dt%c5%len  /= len(dt%c5)  .or. dt%c5%len  /= 7) error stop 24_4
        if(dt%c6%len  /= len(dt%c6)  .or. dt%c6%len  /= 2) error stop 25_4
     end subroutine    

end module

  program typeParamInquiryDTIntrinsic06
  use m
  implicit none

  type(base) :: b1
  type(base),allocatable :: b2
  type(base),pointer :: b3 => null()
  type(base),target  :: b4

  allocate(b2)
  
  b3=>b4 
 
  call b1%getDT
  call b2%getDT
  call b3%getDT
 
end
