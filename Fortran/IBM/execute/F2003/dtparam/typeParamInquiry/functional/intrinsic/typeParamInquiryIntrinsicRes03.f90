!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicRes03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 30  2008  
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. FUNCTION RESULT IS TYPE PARAMETER INQUIRY
!* 4. FUNCTION RESULT IS USED AS ACTUAL ARGUMENT
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicRes03
    implicit none

    interface
       integer function getlen(c)
          character(*),intent(in) :: c
       end function
    end interface

    character(:),allocatable :: c1
    character(3) :: c2="hi" 
    character(:),pointer :: c3(:)=>null()
    integer :: i=0

    allocate(c1,source=c2) 
    if(c1%len /= len(c1) .or. c1%len /= 3)                 error stop 10_4
    call setchar1(c1,3*getlen(c1)) 
    if(c1%len /= len(c1) .or. c1%len /= 9)                 error stop 11_4
    
    allocate(c3(c2%len),source="test")
    if(any(c3 /= "test") )                                 error stop 12_4
    if(c3%len /= len(c3) .or. c3%len /= 4)                 error stop 13_4
    if(ubound(c3,1) /= 3)                                  error stop 14_4

    call setchar2(c3,3*getlen(c2))    
    if(any(c3 /= (/(char(i),i=1,ubound(c3,1))/)) )         error stop 15_4
    if(c3%len /= len(c3) .or. c3%len /= 9)                 error stop 16_4
    if(ubound(c3,1) /= 4)                                  error stop 17_4

    contains

      subroutine setchar1(c,length)
         character(:),allocatable,intent(inout) :: c
         integer :: length

         if(allocated(c))  deallocate(c)
         allocate(character(length) :: c)
      end subroutine

      subroutine setchar2(c,size)
         character(:),pointer,intent(inout) :: c(:)
         integer :: size 

         allocate(character(size) :: c(size/2))
         c=(/(char(i),i=1,size/2)/)
      end subroutine 
end

   integer function getlen(c)
     character(*),intent(in) :: c
     getlen=c%len
   end function
