!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : INTRINSICS(MERGE)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.75
!* 2. INTRINSICS:MERGE(TSOURCE,FSOURCE,MASK)
!* 3. INITIALIZE DERIVED TYPE COMPONENT WITH MERGE
!* 4. INITIALIZE DERIVED TYPE WITH MERGE
!* 5. TSOURCE AND FSOURCE ARE ARRAY
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type :: dtp(k,l)
     integer,kind   :: k=2
     integer,len    :: l=4
     integer(k)     :: i(2)=[merge(k,k,.true.),merge(k+4,k+4,.false.)]
     character(l)   :: c(2)=[merge("123","456",.true.), &
                             merge("000","111",.false.)]
   end type
end module
program mergeAsInitExp02
   use m
   implicit none

   type(dtp),parameter      :: dtp1(2)=[dtp(),dtp()]
   type(dtp(2,4)),parameter :: dtp2(2)= &
     [dtp(2,4)(i=[merge(-1,-2,.false.),merge(-1,-2,.true.)], &
               c=[merge("222","333",.false.),merge("222","333",.true.)]), &
     dtp(2,4)(i=[merge(9,99,.false.),merge(9,99,.true.)], &
              c=[merge("aaa","bbb",.false.),merge("aaa","bbb",.true.)])]

   type(dtp(2,4)) :: dtp3(2)=merge(dtp1,dtp2,[.true.,.false.])

   type(dtp(2,4)) :: dtp4(2)=merge(dtp1,dtp2,[.false.,.true.])

   type(dtp(2,4)) :: dtp5(2)=merge([dtp1(1),dtp2(1)],[dtp1(2),dtp2(2)], &
                                     [.true.,.false.])

   type(dtp(2,4)) :: dtp6(2)=merge([dtp1(1),dtp2(1)],[dtp1(2),dtp2(2)], &
                                     [.false.,.true.])

   if(dtp1%k /= 2)                                  error stop 10_4
   if(dtp1%l /= 4)                                  error stop 11_4
   if(any(dtp1(1)%i /= [2,6]))                      error stop 12_4
   if(any(dtp1(2)%i /= [2,6]))                      error stop 13_4
   if(dtp1(1)%i%kind /= 2)                          error stop 14_4
   if(dtp1(2)%i%kind /= 2)                          error stop 15_4
   if(dtp1(1)%c%len /= 4)                           error stop 16_4
   if(dtp1(2)%c%len /= 4)                           error stop 17_4
   if(any(dtp1(1)%c /= ["123","111"]))              error stop 18_4
   if(any(dtp1(2)%c /= ["123","111"]))              error stop 19_4

   if(dtp2%k /= 2)                                  error stop 20_4
   if(dtp2%l /= 4)                                  error stop 21_4
   if(any(dtp2(1)%i /= [-2,-1]))                    error stop 22_4
   if(any(dtp2(2)%i /= [99,9]))                     error stop 23_4
   if(dtp2(1)%i%kind /= 2)                          error stop 24_4
   if(dtp2(2)%i%kind /= 2)                          error stop 25_4
   if(dtp2(1)%c%len /= 4)                           error stop 26_4
   if(dtp2(2)%c%len /= 4)                           error stop 27_4
   if(any(dtp2(1)%c /= ["333","222"]))              error stop 28_4
   if(any(dtp2(2)%c /= ["bbb","aaa"]))              error stop 29_4

   if(dtp3%k /= 2)                                  error stop 30_4
   if(dtp3%l /= 4)                                  error stop 31_4
   if(any(dtp3(1)%i /= [2,6]))                      error stop 32_4
   if(any(dtp3(2)%i /= [99,9]))                     error stop 33_4
   if(dtp3(1)%i%kind /= 2)                          error stop 34_4
   if(dtp3(2)%i%kind /= 2)                          error stop 35_4
   if(dtp3(1)%c%len /= 4)                           error stop 36_4
   if(dtp3(2)%c%len /= 4)                           error stop 37_4
   if(any(dtp3(1)%c /= ["123","111"]))              error stop 38_4
   if(any(dtp3(2)%c /= ["bbb","aaa"]))              error stop 39_4

   if(dtp4%k /= 2)                                  error stop 40_4
   if(dtp4%l /= 4)                                  error stop 41_4
   if(any(dtp4(1)%i /= [-2,-1]))                    error stop 42_4
   if(any(dtp4(2)%i /= [2,6]))                      error stop 43_4
   if(dtp4(1)%i%kind /= 2)                          error stop 44_4
   if(dtp4(2)%i%kind /= 2)                          error stop 45_4
   if(dtp4(1)%c%len /= 4)                           error stop 46_4
   if(dtp4(2)%c%len /= 4)                           error stop 47_4
   if(any(dtp4(1)%c /= ["333","222"]))              error stop 48_4
   if(any(dtp4(2)%c /= ["123","111"]))              error stop 49_4

   if(dtp5%k /= 2)                                  error stop 50_4
   if(dtp5%l /= 4)                                  error stop 51_4
   if(any(dtp5(1)%i /= [2,6]))                      error stop 52_4
   if(any(dtp5(2)%i /= [99,9]))                     error stop 53_4
   if(dtp5(1)%i%kind /= 2)                          error stop 54_4
   if(dtp5(2)%i%kind /= 2)                          error stop 55_4
   if(dtp5(1)%c%len /= 4)                           error stop 56_4
   if(dtp5(2)%c%len /= 4)                           error stop 57_4
   if(any(dtp5(1)%c /= ["123","111"]))              error stop 58_4
   if(any(dtp5(2)%c /= ["bbb","aaa"]))              error stop 59_4

   if(dtp6%k /= 2)                                  error stop 60_4
   if(dtp6%l /= 4)                                  error stop 61_4
   if(any(dtp6(1)%i /= [2,6]))                      error stop 62_4
   if(any(dtp6(2)%i /= [-2,-1]))                    error stop 63_4
   if(dtp6(1)%i%kind /= 2)                          error stop 64_4
   if(dtp6(2)%i%kind /= 2)                          error stop 65_4
   if(dtp6(1)%c%len /= 4)                           error stop 66_4
   if(dtp6(2)%c%len /= 4)                           error stop 67_4
   if(any(dtp6(1)%c /= ["123","111"]))              error stop 68_4
   if(any(dtp6(2)%c /= ["333","222"]))              error stop 69_4

end program
