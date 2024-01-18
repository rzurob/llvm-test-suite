!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquirySelectTypeParam01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 21 2008 
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
!* 3. SELECT TYPE IN MAIN PROGRAM
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(k1,l1)
      integer(1),kind :: k1=2
      integer(8),len  :: l1=5
      character(k1)   :: c(k1:l1)
      integer(k1)     :: i(l1)
   end type

   type,extends(base)  :: child(k2,l2)
      integer(1),kind :: k2=4
      integer(8),len   :: l2=3
      real(k2)   :: r(l2) 
   end type

end module

  program dtParameterInquirySelectTypeParam01 
  use m
  implicit none
  

  class(base(2,:)),allocatable :: a1
  class(base(4,:)),pointer     :: p1=>null()
  type(child(k1=4,l1=7,k2=8,l2=9)),target :: t1 

  allocate(base() :: a1)
  select type(a1)
    type is(base(2,*))
       print *,"a1 is base"
       if(a1%k1 /=2)                                          error stop 9_4
       if(a1%l1 /=5)                                          error stop 10_4
       if(a1%k1%kind /=kind(a1%k1) .or. a1%k1%kind /= 1)      error stop 11_4
       if(a1%l1%kind /=kind(a1%l1) .or. a1%l1%kind /= 8)      error stop 12_4
       if(a1%c%kind /=kind(a1%c) .or. a1%c%kind /= 1)         error stop 13_4
       if(a1%i%kind /=kind(a1%i) .or. a1%i%kind /= 2)         error stop 14_4 
       if(ubound(a1%c,1) /=a1%l1 .or. lbound(a1%c,1) /=a1%k1) error stop 15_4
       if(ubound(a1%i,1) /=a1%l1 .or. lbound(a1%i,1) /=1)     error stop 16_4
     class is(base(2,*))
       error stop 17_4
     class default
       error stop 18_4 
  end select

  deallocate(a1)

  allocate(child() :: a1)
  select type(a1)
    type is(child(2,*,4,*))
       print *,"a1 is child"
       if(a1%base%k1 /=2)                                     error stop 19_4
       if(a1%l1 /=5)                                          error stop 20_4
       if(a1%base%k1%kind /=kind(a1%base%k1)  &
                       .or. a1%base%k1%kind /= 1)             error stop 21_4
       if(a1%l1%kind /=kind(a1%l1) .or. a1%base%l1%kind /= 8) error stop 22_4
       if(a1%c%kind /=kind(a1%c) .or. a1%base%c%kind /= 1)    error stop 23_4
       if(a1%base%i%kind /=kind(a1%i) .or. a1%i%kind /= 2)    error stop 24_4
       if(ubound(a1%c,1) /=a1%l1 .or. lbound(a1%c,1) /=a1%k1) error stop 25_4
       if(ubound(a1%i,1) /=a1%l1 .or. lbound(a1%i,1) /=1)     error stop 26_4

       if(a1%k2 /=4)                                          error stop 27_4
       if(a1%l2 /=3)                                          error stop 28_4
       if(a1%k2%kind /=kind(a1%k2) .or. a1%k2%kind /= 1)      error stop 29_4
       if(a1%l2%kind /=kind(a1%l2) .or. a1%l2%kind /= 8)      error stop 30_4
       if(a1%r%kind /=kind(a1%r) .or. a1%r%kind /= 4)         error stop 31_4
       if(ubound(a1%r,1) /=a1%l2 .or. lbound(a1%r,1) /=1)     error stop 32_4

     class is(base(2,*))
       error stop 33_4
     class default
       error stop 34_4
  end select

  allocate(base(4,10) :: p1)
  select type(p1)
     type is(base(4,*))
       print *,"p1 is base" 
       if(p1%k1 /=4)                                          error stop 35_4
       if(p1%l1 /=10)                                         error stop 36_4
       if(p1%k1%kind /=kind(p1%k1) .or. p1%k1%kind /= 1)      error stop 37_4
       if(p1%l1%kind /=kind(p1%l1) .or. p1%l1%kind /= 8)      error stop 38_4
       if(p1%c%kind /=kind(p1%c) .or. p1%c%kind /= 1)         error stop 39_4
       if(p1%i%kind /=kind(p1%i) .or. p1%i%kind /= 4)         error stop 40_4
       if(ubound(p1%c,1) /=p1%l1 .or. lbound(p1%c,1) /=p1%k1) error stop 41_4
       if(ubound(p1%i,1) /=p1%l1 .or. lbound(p1%i,1) /=1)     error stop 42_4
     class is(base(4,*))
       error stop 43_4
     class default
       error stop 44_4     
  end select  

  p1=>t1

  select type(p1)
    type is(child(4,*,8,*))
       print *,"p1 is child"
       if(p1%base%k1 /=4)                                     error stop 45_4
       if(p1%l1 /=7)                                          error stop 46_4
       if(p1%base%k1%kind /=kind(p1%base%k1)  &
                       .or. p1%base%k1%kind /= 1)             error stop 47_4
       if(p1%l1%kind /=kind(p1%l1) .or. p1%base%l1%kind /= 8) error stop 48_4
       if(p1%c%kind /=kind(p1%c) .or. p1%base%c%kind /= 1)    error stop 49_4
       if(p1%base%i%kind /=kind(p1%i) .or. p1%i%kind /= 4)    error stop 50_4
       if(ubound(p1%c,1) /=p1%l1 .or. lbound(p1%c,1) /=p1%k1) error stop 51_4
       if(ubound(p1%i,1) /=p1%l1 .or. lbound(p1%i,1) /=1)     error stop 52_4

       if(p1%k2 /=8)                                          error stop 53_4
       if(p1%l2 /=9)                                          error stop 54_4
       if(p1%k2%kind /=kind(p1%k2) .or. p1%k2%kind /= 1)      error stop 55_4
       if(p1%l2%kind /=kind(p1%l2) .or. p1%l2%kind /= 8)      error stop 56_4
       if(p1%r%kind /=kind(p1%r) .or. p1%r%kind /= 8)         error stop 57_4
       if(ubound(p1%r,1) /=p1%l2 .or. lbound(p1%r,1) /=1)     error stop 58_4

     class is(base(4,*))
       error stop 59_4
     class default
       error stop 60_4
  end select


end
