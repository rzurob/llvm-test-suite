!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mergeAsActualArg01.f
!*
!*  DATE                       : Sept. 17 2008
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
!* 3. DERIVED TYPE HAVE DT COMPONENT
!* 4. USE MERGE AS ACTUAL ARGUMENT
!* 5. TSOURCE AND FSOURCE ARE DERIVED TYPE SCALAR
!234567890123456789012345678901234567890123456789012345678901234567890

module m
   type B(k2,l2)
     integer(8),kind  :: k2
     integer(8),len   :: l2

     integer(k2)      :: i2
     character(l2)    :: c2
   end type

   type A(k1,l1)
     integer(2),kind  :: k1
     integer,len      :: l1

     integer(k1)       :: i1
     character(l1)     :: c1

     type(B(2,l1))     :: b1
   end type

   contains

      function getDT1(dt)
         type(A(4,*)),intent(in)  :: dt
         type(A(4,dt%l1))         :: getDT1

         getDT1=dt
      end function
end module

program mergeAsActualArg01
   use m
   implicit none

   type(A(4,:)),pointer     :: a1=>null()
   type(A(4,:)),allocatable :: a2
   type(A(4,5)),target      :: a3

   a3%i1=10
   a3%c1="123"
   a3%b1%i2=20
   a3%b1%c2="456"

   allocate(a1,source=A(4,5)(i1=2*a3%i1, c1="123"(1:2)//"9", &
               b1=B(2,5)(i2=2*a3%b1%i2,  c2="456"(1:2)//"0" ) ) )

   print *,a1%c1%len
   a2=a3

   call check1(getDT1(merge(a1,a2,.true.)))
!   call check2(getDT2(merge(a1%b1,a2%b1,.true.)))
!   call check3(getDT2(merge(a1%b1,a2%b1,.false.)))

   call check2(getDT2(a1%b1,a2%b1,.true.))
   call check3(getDT2(a1%b1,a2%b1,.false.))
   contains

    function getDT2(x,y,flag)
      type(B(2,*)),intent(in) :: x, y
      logical, intent(in) :: flag
      type(B(2,x%l2))      :: getDT2

      if (flag) then
          getDT2 = x
      else
          getDT2 = y
      end if
    end function

    subroutine check1(x)
      type(A(4,*)),intent(in) :: x

      if(x%k1 /= 4)                                  error stop 10_4
      if(x%l1 /= 5)                                  error stop 11_4
      if(x%k1%kind /= 2 .or. kind(x%k1) /= 2)        error stop 12_4
      if(x%l1%kind /= 4 .or. kind(x%l1) /= 4)        error stop 13_4
      if(x%i1 /= 20)                                 error stop 14_4
      if(x%i1%kind /= 4 .or. kind(x%i1) /= 4)        error stop 15_4
      if(x%c1 /= "129")                              error stop 16_4
      if(x%c1%len /= 5 .or. len(x%c1) /= 5)          error stop 17_4
      if(x%b1%k2 /= 2)                               error stop 18_4
      if(x%b1%l2 /= 5)                               error stop 19_4
      if(x%b1%k2%kind /= 8 .or. kind(x%b1%k2) /= 8)  error stop 20_4
      if(x%b1%l2%kind /= 8 .or. kind(x%b1%l2) /= 8)  error stop 21_4
      if(x%b1%i2 /= 40)                              error stop 22_4
      if(x%b1%c2 /= "450")                           error stop 23_4
      if(x%b1%i2%kind /= kind(x%b1%i2) .or.  &
                       x%b1%i2%kind /= 2)            error stop 24_4
      if(x%b1%c2%len /= len(x%b1%c2) .or.  &
                       x%b1%c2%len /= 5)             error stop 25_4

    end subroutine

    subroutine check2(x)
      type(B(2,*)),intent(in) :: x
      if(x%k2 /= 2)                                 error stop 42_4
      print *,x%l2
      if(x%l2 /= 5)                                 error stop 43_4
      if(x%k2%kind /= 8 .or. kind(x%k2) /= 8)       error stop 44_4
      if(x%l2%kind /= 8 .or. kind(x%l2) /= 8)       error stop 45_4
      if(x%i2 /= 40)                                error stop 46_4
      if(x%c2 /= "450")                             error stop 47_4
      if(x%i2%kind /= kind(x%i2) .or.  &
                       x%i2%kind /= 2)              error stop 48_4
      if(x%c2%len /= len(x%c2) .or.  &
                       x%c2%len /= 5)               error stop 49_4
    end subroutine

    subroutine check3(x)
      type(B(2,*)),intent(in) :: x
      if(x%k2 /= 2)                                 error stop 50_4
      if(x%l2 /= 5)                                 error stop 51_4
      if(x%k2%kind /= 8 .or. kind(x%k2) /= 8)       error stop 52_4
      if(x%l2%kind /= 8 .or. kind(x%l2) /= 8)       error stop 53_4
      if(x%i2 /= 20)                                error stop 54_4
      if(x%c2 /= "456")                             error stop 55_4
      if(x%i2%kind /= kind(x%i2) .or.  &
                       x%i2%kind /= 2)              error stop 56_4
      if(x%c2%len /= len(x%c2) .or.  &
                       x%c2%len /= 5)               error stop 57_4
    end subroutine

end program
