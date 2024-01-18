!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryResParam06.f   
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
!* 3. SPECIFICATION FUNCTION AS LENGTH TYPE PARAMETER AND ARRAY BOUND
!* 4. DEFECT 354585 354604
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(l)
       integer,len :: l
    end type

    contains
    pure integer function getlen1(a)
       character(*),intent(in) :: a
          getlen1=a%len 
    end function

    pure integer function getlen3(a)
       type(base(*)),intent(in) :: a
          getlen3=a%l 
    end function

    pure integer function getlen2(a,len)
       character(*),intent(in) :: a
       integer,intent(in) :: len
          getlen2=a%len + len
    end function

    pure integer function getlen4(a,len)
       type(base(*)),intent(in) :: a
       integer,intent(in) :: len
          getlen4=a%l + len
    end function

end module

program dtParameterInquiryResParam06 
  use m
  implicit none

  call test() 
end

subroutine test()
use m

  character(len=:),allocatable :: c1(:)
  character(len=*),parameter :: c2="xlf"
  character(:),pointer :: c3(:)
  character(len=getlen1(c2)) :: c4(getlen1(c2))
  character(getlen2('abc',len(c2))) :: c5(getlen2('abc',len(c2)))


  type(base(:)),allocatable :: t1(:)
  type(base(:)),pointer :: t2(:)
  type(base(3)) :: t3
  type(base(3)),parameter :: t6=base(3)()
  type(base(getlen2('abc',len(c2)))) :: t4(getlen2('abc',len(c2)))
  !--- defect 354604--!
  type(base(getlen4(t6,3))) :: t5(getlen4(t6,3))
  type(base(getlen3(t6))) :: t7(getlen3(t6))
 
  allocate(character(len=getlen1(c2)) :: c1(getlen1(c2)))
  if(c1%len /= 3)                                 error stop 10_4
  if(ubound(c1,1) /= 3)                           error stop 11_4
  allocate(character(len=getlen2("abc",c2%len)) :: c3(getlen2("abc",c2%len)))
  if(c3%len /= 6)                                 error stop 12_4
  if(ubound(c3,1) /= 6)                           error stop 13_4

  if(allocated(t1)) deallocate(t1)
  allocate(base(getlen1(c2)) :: t1(getlen1(c2)) )
  if(t1%l /= 3)                                   error stop 14_4
  if(ubound(t1,1) /= 3)                           error stop 15_4

  allocate(base(getlen2("abc",len(c2))) :: t2(getlen2("abc",len(c2))) )
  if(t2%l /= 6)                                   error stop 16_4
  if(ubound(t2,1) /= 6)                           error stop 17_4

  if(allocated(t1)) deallocate(t1)
  allocate(base(getlen3(t3)) :: t1(getlen3(t3)) )
  if(t1%l /= 3)                                   error stop 18_4
  if(ubound(t1,1) /= 3)                           error stop 19_4

  allocate(base(getlen3(t3)) :: t2(getlen3(t3)) )
  if(t2%l /= 3)                                   error stop 20_4
  if(ubound(t2,1) /= 3)                           error stop 21_4

  if(allocated(t1)) deallocate(t1)
  allocate(base(getlen4(t3,c2%len)) :: t1(getlen4(t3,c2%len)) )
  if(t1%l /= 6)                                   error stop 22_4
  if(ubound(t1,1) /= 6)                           error stop 23_4

  allocate(base(getlen4(t3,c2%len)) :: t2(getlen4(t3,c2%len)) )
  if(t2%l /= 6)                                   error stop 24_4
  if(ubound(t2,1) /= 6)                           error stop 25_4

   if(c4%len /= len(c4) .or. c4%len /= 3)          error stop 26_4
   if(ubound(c4,1) /= 3)                           error stop 27_4

   if(c5%len /= len(c5) .or. c5%len /= 6)          error stop 28_4
   if(ubound(c5,1) /= 6)                           error stop 29_4   

  if(t6%l /= 3)                                   error stop 30_4

  if(t4%l /= 6)                                   error stop 31_4
  if(ubound(t4,1) /= 6)                           error stop 32_4

  if(t5%l /= 6)                                   error stop 33_4
  if(ubound(t5,1) /= 6)                           error stop 34_4

  if(t7%l /= 3)                                   error stop 35_4
  if(ubound(t7,1) /= 3)                           error stop 36_4

 end subroutine
