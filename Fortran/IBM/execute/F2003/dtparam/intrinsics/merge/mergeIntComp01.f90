!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeIntComp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 9 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
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
!* 1. TEST SECTION 13.7.75 
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK) 
!* 3. TSOURCE,FSOURCE ARE SCALAR DERIVED TYPE
!* 4. COMPONENT ARE SCALAR REAL,INTEGER,COMPLEX
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(k,l)
     integer(2),kind :: k=8
     integer(8),len  :: l=3
    
     integer(2) :: i1=234 
     integer(k) :: i2=-456
     real(4)    :: r1=-2.33
     real(k)    :: r2=3.2E-2 
     complex(k) :: x=(1.2,3.5) 
  end type
end module

program mergeIntComp01
   use m
   implicit none

   logical precision_r4,precision_r8,precision_x16 

   type(A) :: a1 
   type(A) :: a2=A()(0,0,0.,0._8,(0._8,0._8)) 
   type(A),parameter :: a3=A(8,3)(9,99,9._4,99._8,(-9._8,-9._8)) 

   a2=merge(a1,a2,.true.)

   if(a2%k /= a1%k .or. a2%k /=8)                          error stop 10_4
   if(a2%l /= a1%l .or. a2%l /=3)                          error stop 11_4
   if(a2%i1 /= a1%i1 .or. a2%i1/= 234)                     error stop 12_4
   if(a2%i1%kind /= a1%i1%kind .or. a2%i1%kind /= 2)       error stop 13_4
   if(a2%i2 /= a1%i2 .or. a2%i2 /= -456)                   error stop 14_4
   if(a2%i2%kind /= a1%i2%kind .or. a2%i2%kind /= 8)       error stop 15_4
   if(a2%r1%kind /= a1%r1%kind .or. a2%r1%kind /= 4)       error stop 16_4
   if(a2%r2%kind /= a1%r2%kind .or. a2%r2%kind /= 8)       error stop 17_4 
   if(.not. precision_r4(a2%r1,a1%r1))                     error stop 18_4
   if(.not. precision_r8(a2%r2,a1%r2))                     error stop 19_4 
   if(.not. precision_x16(a2%x,a1%x))                      error stop 20_4  

   a1=merge(a1,A()(1,2,3._4,4._8,(5._8,5._8)),.false.)

   if(a1%k /= 8 )                                          error stop 21_4
   if(a1%l /= 3)                                           error stop 22_4
   if(a1%i1 /= 1)                                          error stop 23_4
   if(a1%i1%kind /= 2)                                     error stop 24_4
   if(a1%i2 /= 2)                                          error stop 25_4
   if(a1%i2%kind /= 8)                                     error stop 26_4
   if(a1%r1%kind /= 4)                                     error stop 27_4
   if(a1%r2%kind /= 8)                                     error stop 28_4
   if(.not. precision_r4(a1%r1,3._4))                      error stop 29_4
   if(.not. precision_r8(a1%r2,4._8))                      error stop 30_4
   if(.not. precision_x16(a1%x,(5._8,5._8)))               error stop 31_4


   call sub1(a1)

   if(a1%k /= 8 )                                          error stop 60_4
   if(a1%l /= 3)                                           error stop 61_4
   if(a1%i1 /= 9)                                          error stop 62_4
   if(a1%i1%kind /= 2)                                     error stop 63_4
   if(a1%i2 /= 99)                                         error stop 64_4
   if(a1%i2%kind /= 8)                                     error stop 65_4
   if(a1%r1%kind /= 4)                                     error stop 66_4
   if(a1%r2%kind /= 8)                                     error stop 67_4
   if(.not. precision_r4(a1%r1,9._4))                      error stop 68_4
   if(.not. precision_r8(a1%r2,99._8))                     error stop 69_4
   if(.not. precision_x16(a1%x,(-9._8,-9._8)))             error stop 70_4

   contains

     subroutine sub1(a)
       type(A(8,3)),intent(inout) :: a

       if(a%k /= 8 )                                       error stop 40_4
       if(a%l /= 3)                                        error stop 41_4
       if(a%i1 /= 1)                                       error stop 42_4
       if(a%i1%kind /= 2)                                  error stop 43_4
       if(a%i2 /= 2)                                       error stop 44_4
       if(a%i2%kind /= 8)                                  error stop 45_4
       if(a%r1%kind /= 4)                                  error stop 46_4
       if(a%r2%kind /= 8)                                  error stop 47_4
       if(.not. precision_r4(a%r1,3._4))                   error stop 48_4
       if(.not. precision_r8(a%r2,4._8))                   error stop 49_4
       if(.not. precision_x16(a%x,(5._8,5._8)))            error stop 50_4

       a = merge(a3,a,(a%l .eq. 3))

     end subroutine

end program
