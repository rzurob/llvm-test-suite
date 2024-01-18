!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicAssum01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 8 2008 
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
!* 3. DUMMY ARGUMENT HAS ASSUMED LENGTH AND HAS INTENT(IN) ATTRIBUTE
!* 4. DEFECT 352994,354606,354812
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   character(:),pointer :: a
   character(:),allocatable :: b
   character(:),pointer :: c(:)
   character(:),allocatable :: d(:)
   character(len=7),target :: e="xlftest"
   character(len=*),parameter :: f(3)=['abcd','efgh','ijkl'] 
end module
program typeParamInquiryIntrinsicAssum01
    use m
    implicit none

    allocate(a,source="xlf"//"test") 
    b="xlf"//"test"

    call test1(a)
    call test1(b)
    call test1(e)

    call test2(a(1:3))
    call test2(b(1:3))
    call test2(e(1:3))


    allocate(c(size(f)),source=f)
    d=c 
    
    call test3(c)
    call test3(d)

     call test4(c(1:2)(1:3))
     call test4(d(1:2)(1:3))
     call test4(f(1:2)(1:3))

    contains

      subroutine test1(arg)
         character(*),intent(in) :: arg 
         
           if(arg%len /= len(arg) .or. arg%len /=7)          error stop 10_4 
           if(arg%kind /= kind(arg) .or. arg%kind /=1)       error stop 11_4
           if(len(arg(1:3))    /=3)                          error stop 12_4
           if(arg /= "xlftest")                              error stop 13_4
      end subroutine         

      subroutine test2(arg)
         character(*),intent(in) :: arg

           print *,arg%len,len(arg)
           if(arg%len /= len(arg) .or. arg%len /=3)          error stop 14_4
           if(arg%kind /= kind(arg) .or. arg%kind /=1)       error stop 15_4
           if(len(arg(1:0))    /=0)                          error stop 16_4
           if(arg /= "xlf")                                  error stop 17_4

      end subroutine

     subroutine test3(arg)
        character(*),intent(in) :: arg(2:)

          if(arg%len /= len(arg) .or. arg%len /=4)           error stop 18_4
          if(arg%kind /= kind(arg) .or. arg%kind /=1)        error stop 19_4
          if(ubound(arg,1) /= 4)                             error stop 20_4
          if(any(arg /= ['abcd','efgh','ijkl']))             error stop 21_4

     end subroutine

     subroutine test4(arg)
        character(*),intent(in) :: arg(2:)

           print *,arg%len,len(arg),arg
          if(arg%len /= len(arg) .or. arg%len /=3)           error stop 22_4
          if(arg%kind /= kind(arg) .or. arg%kind /=1)        error stop 23_4
          if(ubound(arg,1) /= 3)                             error stop 24_4
          if(any(arg /= ['abc','efg']))                      error stop 25_4
 
     end subroutine      

end

