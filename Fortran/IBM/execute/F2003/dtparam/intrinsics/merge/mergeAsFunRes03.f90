!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeAsFunRes03.f   
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
!* 3. TSOURCE AND FSOURCE ARE POLYMORPHIC
!* 4. FUNCTION RESULT ARE MERGE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind  :: k1
     integer,len   :: l1
     integer(k1)   :: i1
     character(l1) :: c1 
  end type 
  type,extends(base) :: child(k2,l2) 
     integer,kind  :: k2
     integer,len   :: l2
     integer(k2)   :: i2
     character(l2) :: c2 
  end type 
  contains

     function getMergeResult(Ts,Fs,Mask)
        class(base(2,*)),intent(in) :: Ts,Fs
        logical,intent(in)        :: Mask
        class(base(2,:)),pointer :: getMergeResult
        select type(Ts) 
           type is(child(2,*,4,*))
             select type(Fs)
               type is(child(2,*,4,*))
                  allocate(getMergeResult,source=merge(Ts,Fs,Mask))
               class default
                  error stop 101_4
             end select
           class default
              error stop 100_4
        end select
     end function
end module

program mergeAsFunRes03
   use m
   implicit none

   class(base(2,:)),allocatable :: ba1,ba2
 
    
    
   allocate(ba1,source=child(2,3,4,7) (i1=1, c1= "123", &
                                       i2=11 , c2= "456") )


   allocate(ba2,source=child(2,3,4,7) (i1=3, c1= "aaa", &
                                       i2=33, c2= "bbb") )

   select type(x=>getMergeResult(ba1,ba2,.true.) )
         type is(child(2,*,4,*))
           if(x%k1 /= 2)                           error stop 10_4
           if(x%l1 /= 3)                           error stop 11_4
           if(x%k2 /= 4)                           error stop 12_4
           if(x%l2 /= 7)                           error stop 14_4
           if(x%i1 /= 1)                           error stop 15_4
           if(x%i1%kind /= 2)                      error stop 16_4
           if(x%i2 /= 11)                          error stop 17_4
           if(x%i2%kind /= 4)                      error stop 18_4
           if(x%c1 /= "123")                       error stop 19_4
           if(x%c2 /= "456")                       error stop 20_4
        class default 
           error stop 102_4           
   end select

   select type(x=>getMergeResult(ba1,ba2,.false.) )
         type is(child(2,*,4,*))
           if(x%k1 /= 2)                           error stop 21_4
           if(x%l1 /= 3)                           error stop 22_4
           if(x%k2 /= 4)                           error stop 23_4
           if(x%l2 /= 7)                           error stop 24_4
           if(x%i1 /= 3)                           error stop 25_4
           if(x%i1%kind /= 2)                      error stop 26_4
           if(x%i2 /= 33)                          error stop 27_4
           if(x%i2%kind /= 4)                      error stop 28_4
           if(x%c1 /= "aaa")                       error stop 29_4
           if(x%c2 /= "bbb")                       error stop 30_4
         class default
           error stop 103_4
   end select      
  
end program

