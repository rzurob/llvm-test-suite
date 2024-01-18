!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22 2008
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
!* 3. COMPONENTS ARE POLYMORPHIC ALLOCATABLE OR POINTER
!* 4. DEFECT 356156
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(k1,l1)
      integer,kind :: k1
      integer,len  :: l1

      integer(2*k1) :: i1
      character(2*l1) :: c1
   end type

   type B(k2,l2)
      integer,kind :: k2
      integer,len  :: l2

      class(A(k2,:)),allocatable :: a1
      class(A(k2,:)),pointer     :: a2=>null()
   end type
end module

program mergePolyComp01
   use m
   implicit none

   type(B(2,3)),target      :: b1
   type(B(2,:)),allocatable :: b2
   type(B(2,:)),pointer     :: b3=>null()

   allocate(b1%a1,source=A(2,3)(i1=1,c1="123"))
   allocate(b1%a2,source=A(2,3)(i1=2,c1="456"))

   allocate(b2,source=B(2,3)(null(),null()))
   allocate(b2%a1,source=A(2,3)(i1=-1,c1="000"))
   allocate(b2%a2,source=A(2,3)(i1=-2,c1="111"))

   b3=>b1

   call associate10_23(merge(b1,b2,.true.))

   call associate24_37(merge(b1,b2,.false.))

   call associate38_63(merge([b1,b2],[b2,b1],.false.))

   call associate64_77(merge(b3,b2,.true.))

   contains

!   associate(x=>merge(b1,b2,.true.))
   subroutine associate10_23(x)
       type(b(2,*)), intent(in) :: x

       if(x%k2 /= 2)                              error stop 10_4
       if(x%l2 /= 3)                              error stop 11_4

       select type(y=>x%a1)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 12_4
              if(y%l1 /= 3)                       error stop 13_4
              if(y%i1%kind /= 4)                  error stop 14_4
              if(y%c1%len /= 6)                   error stop 15_4
              if(y%i1 /= 1)                       error stop 16_4
              if(y%c1 /= "123")                   error stop 17_4
           class default
             error stop 100_4
       end select

       select type(y=>x%a2)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 18_4
              if(y%l1 /= 3)                       error stop 19_4
              if(y%i1%kind /= 4)                  error stop 20_4
              if(y%c1%len /= 6)                   error stop 21_4
              if(y%i1 /= 2)                       error stop 22_4
              if(y%c1 /= "456")                   error stop 23_4
          class default
             error stop 101_4
       end select
   end subroutine

!   associate(x=>merge(b1,b2,.false.))
   subroutine associate24_37(x)
       type(b(2,*)), intent(in) :: x

       if(x%k2 /= 2)                              error stop 24_4
       if(x%l2 /= 3)                              error stop 25_4

       select type(y=>x%a1)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 26_4
              if(y%l1 /= 3)                       error stop 27_4
              if(y%i1%kind /= 4)                  error stop 28_4
              if(y%c1%len /= 6)                   error stop 29_4
              if(y%i1 /= -1)                      error stop 30_4
              if(y%c1 /= "000")                   error stop 31_4
          class default
              error stop 102_4
       end select

       select type(y=>x%a2)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 32_4
              if(y%l1 /= 3)                       error stop 33_4
              if(y%i1%kind /= 4)                  error stop 34_4
              if(y%c1%len /= 6)                   error stop 35_4
              if(y%i1 /= -2)                      error stop 36_4
              if(y%c1 /= "111")                   error stop 37_4
          class default
              error stop 103_4
       end select

   end subroutine

!   associate(x=>merge([b1,b2],[b2,b1],.false.))
   subroutine associate38_63(x)
      type(b(2,*)), intent(in) :: x(:)

       if(x%k2 /= 2)                              error stop 38_4
       if(x%l2 /= 3)                              error stop 39_4

       select type(y=>x(1)%a1)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 40_4
              if(y%l1 /= 3)                       error stop 41_4
              if(y%i1%kind /= 4)                  error stop 42_4
              if(y%c1%len /= 6)                   error stop 43_4
              if(y%i1 /= -1)                      error stop 44_4
              if(y%c1 /= "000")                   error stop 45_4
           class default
              error stop 104_4
       end select

       select type(y=>x(1)%a2)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 46_4
              if(y%l1 /= 3)                       error stop 47_4
              if(y%i1%kind /= 4)                  error stop 48_4
              if(y%c1%len /= 6)                   error stop 49_4
              if(y%i1 /= -2)                      error stop 50_4
              if(y%c1 /= "111")                   error stop 51_4
          class default
              error stop 105_4
       end select

       select type(y=>x(2)%a1)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 52_4
              if(y%l1 /= 3)                       error stop 53_4
              if(y%i1%kind /= 4)                  error stop 54_4
              if(y%c1%len /= 6)                   error stop 55_4
              if(y%i1 /= 1)                       error stop 56_4
              if(y%c1 /= "123")                   error stop 57_4
           class default
              error stop 106_4
       end select

       select type(y=>x(2)%a2)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 58_4
              if(y%l1 /= 3)                       error stop 59_4
              if(y%i1%kind /= 4)                  error stop 60_4
              if(y%c1%len /= 6)                   error stop 61_4
              if(y%i1 /= 2)                       error stop 62_4
              if(y%c1 /= "456")                   error stop 63_4
          class default
              error stop 107_4
       end select

   end subroutine

!   associate(x=>merge(b3,b2,.true.))
   subroutine associate64_77(x)
       type(b(2,*)), intent(in) :: x

       if(x%k2 /= 2)                              error stop 64_4
       if(x%l2 /= 3)                              error stop 65_4

       select type(y=>x%a1)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 66_4
              if(y%l1 /= 3)                       error stop 67_4
              if(y%i1%kind /= 4)                  error stop 68_4
              if(y%c1%len /= 6)                   error stop 69_4
              if(y%i1 /= 1)                       error stop 70_4
              if(y%c1 /= "123")                   error stop 71_4
          class default
              error stop 108_4
       end select

       select type(y=>x%a2)
          type is(A(2,*))
              if(y%k1 /= 2)                       error stop 72_4
              if(y%l1 /= 3)                       error stop 73_4
              if(y%i1%kind /= 4)                  error stop 74_4
              if(y%c1%len /= 6)                   error stop 75_4
              if(y%i1 /= 2)                       error stop 76_4
              if(y%c1 /= "456")                   error stop 77_4
           class default
              error stop 109_4
       end select

   end subroutine
end program
