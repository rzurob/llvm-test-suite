!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeAsInitExp01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 19 2008 
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
!* 3. INITIALIZE DERIVED TYPE COMPONENT WITH MERGE
!* 4. INITIALIZE DERIVED TYPE WITH MERGE
!* 5. TSOURCE AND FSOURCE ARE SCALAR
!* 6  DEFECT 356488  
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp(k,l)
     integer,kind   :: k=merge(4,1,.true.)
     integer,len    :: l=merge(0,3,.false.)    
     integer(k)     :: i=merge(1,2,.true.)
     character(l)   :: c=merge("xlf","123",.true.)
   end type
end module
program mergeAsInitExp01
   use m
   implicit none

   type(dtp),parameter      :: dtp1=dtp()
   type(dtp(4,3)),parameter :: dtp2=dtp(4,3)(i=merge(-1,-2,.false.), &
                                   c=merge("000","111",.false.))

   type(dtp(4,3)) :: dtp3=merge(dtp1,dtp2,.true.) 

   type(dtp(4,3)) :: dtp4=merge(dtp1,dtp2,.false.)

   if(dtp1%k /= 4)                             error stop 10_4
   if(dtp1%l /= 3)                             error stop 11_4
   if(dtp1%i /= 1)                             error stop 12_4
   if(dtp1%i%kind /= 4)                        error stop 13_4
   if(dtp1%c /= "xlf")                         error stop 14_4
   if(dtp1%c%len /= 3)                         error stop 15_4

   if(dtp2%k /= 4)                             error stop 16_4
   if(dtp2%l /= 3)                             error stop 17_4
   if(dtp2%i /= -2)                            error stop 18_4
   if(dtp2%i%kind /= 4)                        error stop 19_4
   if(dtp2%c /= "111")                         error stop 20_4
   if(dtp2%c%len /= 3)                         error stop 21_4   

   if(dtp3%k /= 4)                             error stop 22_4
   if(dtp3%l /= 3)                             error stop 23_4
   if(dtp3%i /= 1)                             error stop 24_4
   if(dtp3%i%kind /= 4)                        error stop 25_4
   if(dtp3%c /= "xlf")                         error stop 26_4
   if(dtp3%c%len /= 3)                         error stop 27_4

   if(dtp4%k /= 4)                             error stop 28_4
   if(dtp4%l /= 3)                             error stop 29_4
   if(dtp4%i /= -2)                            error stop 30_4
   if(dtp4%i%kind /= 4)                        error stop 31_4
   if(dtp4%c /= "111")                         error stop 32_4
   if(dtp4%c%len /= 3)                         error stop 33_4

end program
