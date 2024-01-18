!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeDTComp05.f
!*
!*  DATE                       : Sept. 16 2008
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
!* 3. DERIVED TYPE  HAS EXTENED TYPE
!* 4. EXTENDED DRIVED TYPE HAS POLYMORPHIC TYPE
!* 5. TSOURCE AND FSOURCE ARE POLYMORPHIC TYPE
!* 6. USE ASSOCIATE
!* 7. DEFECT 355334 356228
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(k1,l1)
       integer,kind  :: k1
       integer,len   :: l1
       character(l1) :: c1(k1)
    end type
    type,extends(base) :: child(l2)
       integer,len   :: l2
       character(l2) :: c2
       class(base(2,:)),pointer     :: base1=>null()
    end type

end module

program mergeDTComp05
   use m
   implicit none

   class(base(2,:)),allocatable  :: b1
   class(base(2,:)),pointer      :: b2
   type(child(2,3,4)),target     :: ch1,ch2

   ch1=child(2,3,4)(c1=["ab","cd"],c2="test")
   ch2=child(2,3,4)(c1=["12","34"],c2="xyz")

   ch1%base1=>ch2
   ch2%base1=>ch1

   b2=>ch2
   allocate(b1,source=ch1)

   associate(x=>merge(b1,b2,.true.))
       select type(x)
           type is(child(2,*,*))
!               print *,x%c1,any(x%c1 /= ["ab","cd"])
               if(x%k1 /= 2)                              error stop 10_4
               if(x%l1 /= 3)                              error stop 11_4
               if(x%c1%len /= 3)                          error stop 12_4
               if(size(x%c1,1) /= 2)                      error stop 13_4
               if(x%l2 /= 4)                              error stop 14_4
               if(x%c2%len /= 4)                          error stop 15_4
               !--- defect 355334--!
               if(any(x%c1 /= ["ab","cd"]))               error stop 16_4
               !--- defect 356228---!
               if(x%c2 /= "test")                         error stop 17_4
               associate(y=>x%base1)
                   select type(y)
                      type is(child(2,*,*))
                          if(y%l1 /= 3)                   error stop 18_4
                          if(y%l2 /= 4)                   error stop 19_4
                          if(any(y%c1 /= ["12","34"]))    error stop 20_4
                          if(y%c2 /= "xyz")               error stop 21_4
                       class default
                          error stop 101_4
                   end select
               end associate
            class default
               error stop 100_4
       end select
   end associate

   associate(x=>merge(b1,b2,.false.))
       select type(x)
           type is(child(2,*,*))
               print *,x%c1
               if(x%k1 /= 2)                              error stop 22_4
               if(x%l1 /= 3)                              error stop 23_4
               if(x%c1%len /= 3)                          error stop 24_4
               if(size(x%c1,1) /= 2)                      error stop 25_4
               if(x%l2 /= 4)                              error stop 26_4
               if(x%c2%len /= 4)                          error stop 27_4
               !--- defect 355334--!
               if(any(x%c1 /= ["12","34"]))               error stop 28_4
               if(x%c2 /= "xyz")                          error stop 29_4
               associate(y=>x%base1)
                   select type(y)
                      type is(child(2,*,*))
                          if(y%l1 /= 3)                   error stop 30_4
                          if(y%l2 /= 4)                   error stop 31_4
                          if(any(y%c1 /= ["ab","cd"]))    error stop 32_4
                          if(y%c2 /= "test")              error stop 33_4
                      class default
                          error stop 103_4
                   end select
               end associate
            class default
               error stop 102_4
       end select
   end associate

   select type(x=>b1)
       type is(child(2,*,*))
          select type(y=>merge(x%base1,x%base1,.true.))
             type is(child(2,*,*))
                if(y%l1 /=3)                             error stop 34_4
                if(y%l2 /= 4)                            error stop 35_4
                if(any(y%c1 /= ["12","34"]))             error stop 36_4
                if(y%c2 /= "xyz")                        error stop 37_4
             class default
                error stop 105_4
          end select
       class default
           error stop 104_4
   end select

   select type(x=>b2)
       type is(child(2,*,*))
          select type(y=>merge(x%base1,x%base1,.true.))
             type is(child(2,*,*))
                if(y%l1 /=3)                             error stop 38_4
                if(y%l2 /= 4)                            error stop 39_4
                if(any(y%c1 /= ["ab","cd"]))             error stop 40_4
                if(y%c2 /= "test")                       error stop 41_4
             class default
                error stop 107_4
          end select
       class default
          error stop 106_4
   end select


end program
