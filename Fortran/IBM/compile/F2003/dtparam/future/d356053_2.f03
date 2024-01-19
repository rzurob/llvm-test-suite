!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 13 2008
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
  type,extends(A) :: B
  end type
end module

program d356053_2
   use m
   implicit none

   class(A),pointer :: a1
   type(A) :: a2
   type(B),target   :: b1
   class(*),pointer :: c1

   allocate(a1,source=b1)
   allocate(c1,source=a1)
   call sub1(merge(a1,a2,.true.)) ! a1 is class(A),a2 is type(A)
   call sub1(merge(a1,b1,.true.)) ! a1 is class(A),b1 is type(B)
   call sub1(merge(a1,c1,.true.)) ! a1 is class(A),b1 is unlimited polymorphic
   call sub2(merge(c1,a2,.true.)) ! c1 is unlimited polymophic,a2 is type(A)
   call sub2(merge(c1,b1,.true.)) ! c1 is unlimited polymophic,b1 is type(B)
   call sub2(merge(c1,a1,.true.)) ! c1 is unlimited polymophic,b1 is class(A)

   contains
     subroutine sub1(dt)
        class(A),intent(in) :: dt
     end subroutine
     subroutine sub2(dt)
        class(*),intent(in) :: dt
     end subroutine
end program
