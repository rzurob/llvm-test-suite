!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeCharComp04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 12 2008 
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
!* 3. TSOURCE,FSOURCE ARE POLYMORPHIC  
!* 4. DERIVED TYPE HAS CHARACTER ARRAY POINTER COMPONENT 
!* 5. DERIVED TYPE HAS EXTENDED TYPE 
!* 6. DEFECT 356111
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l1)
     integer,len  :: l1=3
     character(:),pointer :: c1(:)=>null()
  end type
  type,extends(A) :: B(l2)
     integer(2),len :: l2=4
     character(l1+l2) :: c2="xlftest" 
  end type
end module

program mergeCharComp04
   use m
   implicit none

   class(A),pointer ::a1
   class(A),allocatable :: b1     
   class(*),pointer :: c1 

   class(A(:)),pointer      :: a2(:)
   class(A(:)),allocatable  :: b2(:)
   class(*),pointer         :: c2(:)

   character(7),target      :: ch1(4)=["abcd","efgh","ijkl","mnop"]
   
   allocate(a1,source=B(3,5)(ch1,"aaa"))
   allocate(b1,source=B(3,5)(ch1(1:2),"bbb"))
   c1=>a1

   allocate(a2(2),source=[B(4,6)(ch1(1:2),"aa1"),B(4,6)(ch1(3:4),"aa2")])
   
   allocate(b2(2),source=[B(4,6)(ch1(3:3)(2:3),"bb1"),B(4,6)(ch1(4:4)(2:3),"bb2")]) 
   c2=>a2 
 
 
   select type(x=>merge(a1,b1,.true.))
      type is(B(*,*))
         if(x%l1 /= 3)                                error stop 10_4
         if(x%l2 /= 5)                                error stop 11_4
         if(any(x%c1 /= a1%c1))                       error stop 12_4
         if(x%c2 /= "aaa")                            error stop 13_4
         if(x%c1%len /= 7)                            error stop 14_4
         if(x%c2%len /= 8)                            error stop 15_4
         if(size(x%c1,1) /= 4)                        error stop 16_4
      class is(B(*,*))
         error stop 100_4
      class default
         error stop 101_4
   end select

   select type(x=>merge(a1,b1,.false.))
      type is(B(*,*))
         if(x%l1 /= 3)                                error stop 17_4
         if(x%l2 /= 5)                                error stop 18_4
         if(any(x%c1 /= b1%c1))                       error stop 19_4
         if(x%c2 /= "bbb")                            error stop 20_4
         if(x%c1%len /= 7)                            error stop 21_4
         if(x%c2%len /= 8)                            error stop 22_4
         if(x%c1%len /= 7)                            error stop 23_4
         if(x%c2%len /= 8)                            error stop 24_4
         if(size(x%c1,1) /= 2)                        error stop 25_4
      class is(B(*,*))
         error stop 102_4
      class default
         error stop 103_4
   end select

   select type(x=>merge(c1,a1,.true.))
      type is(B(*,*))
         if(x%l1 /= 3)                                error stop 26_4
         if(x%l2 /= 5)                                error stop 27_4
         if(any(x%c1 /= a1%c1))                       error stop 28_4
         if(x%c2 /= "aaa")                            error stop 29_4
         if(x%c1%len /= 7)                            error stop 30_4
         if(x%c2%len /= 8)                            error stop 31_4
         if(size(x%c1,1) /= 4)                        error stop 32_4
      class is(B(*,*))
         error stop 104_4
      class default
         error stop 105_4
   end select

   select type(x=>merge(b1,c1,.false.))
      type is(B(*,*))
         if(x%l1 /= 3)                                error stop 33_4
         if(x%l2 /= 5)                                error stop 34_4
         if(any(x%c1 /= a1%c1))                       error stop 35_4
         if(x%c2 /= "aaa")                            error stop 36_4
         if(x%c1%len /= 7)                            error stop 37_4
         if(x%c2%len /= 8)                            error stop 38_4
         if(size(x%c1,1) /= 4)                        error stop 39_4
      class is(B(*,*))
         error stop 106_4
      class default
         error stop 107_4
   end select

   select type(x=>merge(a2,b2,.true.))
      type is(B(*,*))
         if(x%l1 /= 4)                                error stop 40_4
         if(x%l2 /= 6)                                error stop 41_4
         if(any(x(1)%c1 /= ["abcd","efgh"]))          error stop 42_4
         if(any(x(2)%c1 /= ["ijkl","mnop"]))          error stop 43_4 
         if(x(1)%c1%len /= 7)                         error stop 44_4
         if(x(2)%c1%len /= 7)                         error stop 45_4
         if(any(x%c2 /= ["aa1","aa2"]))               error stop 46_4
          if(x%c2%len /= 10)                          error stop 47_4          
      class is(B(*,*))
         error stop 108_4
      class default
         error stop 109_4
   end select

   select type(x=>merge(a2,b2,.false.))
      type is(B(*,*))
         if(x%l1 /= 4)                                error stop 48_4
         if(x%l2 /= 6)                                error stop 49_4
         if(any(x(1)%c1 /= ["jk"]))                   error stop 50_4
         if(any(x(2)%c1 /= ["no"]))                   error stop 51_4
         if(x(1)%c1%len /= 2)                         error stop 52_4
         if(x(2)%c1%len /= 2)                         error stop 53_4
         if(any(x%c2 /= ["bb1","bb2"]))               error stop 54_4
          if(x%c2%len /= 10)                          error stop 55_4
      class is(B(*,*))
         error stop 110_4
      class default
         error stop 111_4
   end select

   select type(x=>merge(a2,b2,[.false.,.true.]))
      type is(B(*,*))
         if(x%l1 /= 4)                                error stop 56_4
         if(x%l2 /= 6)                                error stop 57_4
         if(any(x(1)%c1 /= ["jk"]))                   error stop 58_4
         if(any(x(2)%c1 /= ["ijkl","mnop"]))          error stop 59_4
         if(x(1)%c1%len /= 2)                         error stop 60_4
         if(x(2)%c1%len /= 7)                         error stop 61_4
         if(any(x%c2 /= ["bb1","aa2"]))               error stop 62_4
         if(x%c2%len /= 10)                           error stop 63_4
      class is(B(*,*))
         error stop 112_4
      class default
         error stop 113_4
   end select

   select type(x=>merge(a2,b2,[.true.,.false.]))
      type is(B(*,*))
         if(x%l1 /= 4)                                error stop 64_4
         if(x%l2 /= 6)                                error stop 65_4
         if(any(x(1)%c1 /= ["abcd","efgh"]))          error stop 66_4
         if(any(x(2)%c1 /= ["no"]))                   error stop 67_4
         if(x(1)%c1%len /= 7)                         error stop 68_4
         if(x(2)%c1%len /= 2)                         error stop 69_4
         if(any(x%c2 /= ["aa1","bb2"]))               error stop 70_4
         if(x%c2%len /= 10)                           error stop 71_4
      class is(B(*,*))
         error stop 114_4
      class default
         error stop 115_4
   end select


end program
