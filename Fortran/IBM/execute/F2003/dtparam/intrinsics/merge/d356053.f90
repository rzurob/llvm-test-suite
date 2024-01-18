!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 11 2008
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
!* 3. DEFECT 356053
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A
  end type
  type,extends(A) :: B(l)
     integer,len :: l=4
  end type
end module

program d356053
   use m
   implicit none

   class(A),pointer ::a1
   class(A),allocatable  ::b1
   class(A),pointer :: c1
   type(B),target :: d1

   allocate(b1,source=d1)
   allocate(a1,source=b1)
   allocate(c1,source=merge(a1,b1,.true.))

   select type(x=>c1)
      type is(B(*))
         print *,x%l
      class default
         error stop 100_4
   end select
   select type(x=>a1)
      type is(B(*))
         print *,x%l
      class default
         error stop 101_4
   end select
end program
