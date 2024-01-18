!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicRes04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : August 5 2008  
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
!* 3. FUNCTON RESULT IS CHARACTER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    character(:),allocatable :: c3(:),c5(:)
    character(:),pointer  :: c4

end module

program typeParamInquiryIntrinsicRes04
    use m
    implicit none
    
    character(len=*),parameter :: c1="abc"
    character(len=3)  :: c2="abc" 

    if(getchar1('abc') /= 'xlftest')            error stop 10_4
    if(getchar1(c1) /= 'xlftest')               error stop 11_4
    if(getchar1(c2) /= 'xlftest')               error stop 12_4
    if(len(getchar1('abc')) /= 9)               error stop 13_4 
    if(len(getchar1(c1)) /= 9)                  error stop 14_4
    if(len(getchar1(c2)) /= 9)                  error stop 15_4

    if(getchar2('abc') /= 'fortran test team')  error stop 16_4
    if(getchar2(c1) /= 'fortran test team')     error stop 17_4
    if(getchar2(c2) /= 'fortran test team')     error stop 18_4
    if(len(getchar2('abc')) /= 18)              error stop 19_4
    if(len(getchar2(c1)) /= 18)                 error stop 20_4
    if(len(getchar2(c2)) /= 18)                 error stop 21_4

    allocate(c4,source=getchar3('xlf fortran'(5:),'test team'(5:9)))   
    if(c4%len /= len('fortran team') .or. &
       len(c4) /= len('fortran team') )         error stop 22_4
    if(c4%kind /= kind(c4) .or. c4%kind /= 1)   error stop 23_4
    if(c4 /= 'fortran team')                    error stop 24_4

    allocate(c3(len('xlf'):len('test')),source=getchar3('xlf','test'))

    if(ubound(c3,1) /= 4 .or. lbound(c3,1) /= 3)  error stop 25_4
    if(c3%len /=len(c3) .or. c3%len /= 7)         error stop 26_4
    if(any(c3 /= 'xlftest') )                     error stop 27_4

    allocate( c5(size(getchar4(c3,' team'))),source=getchar4(c3,' team')) 

    if(c5%len /=len(c5) .or. c5%len /= 12)        error stop 28_4
    if(c5%kind /=kind(c5) .or. c5%kind /= 1)      error stop 29_4
    if(any(c5 /= 'xlftest team'))                 error stop 30_4
    if(ubound(c5,1) /= 2)                         error stop 31_4

    contains

      function getchar1(c)
         character(*),intent(in) :: c
         character(3*c%len) :: getchar1
         getchar1='xlftest'
      end function

     function getchar2(c)
         character(*),intent(in) :: c
         character(:),pointer :: getchar2
         allocate(character(3*(c%len+len(c)))  :: getchar2)
         getchar2='fortran test team'
     end function

     function getchar3(a,b)
         character(*),intent(in) :: a,b
         character(:),pointer :: getchar3

         allocate(getchar3,source=a//b)
     end function

     character(:) function getchar4(a,b)
         character(*),intent(in) :: a(:),b
         allocatable :: getchar4(:)

         allocate(getchar4(size(a)),source=a//b)
     end function
      
end

