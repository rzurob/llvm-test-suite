!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356053_3.f
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
  type:: B(l)
     integer,len :: l=4
  end type
end module

program d356053_3
   use m
   implicit none

   class(B),pointer :: b1
   type(B),target   :: b2
   class(*),pointer :: c1
   integer,target   :: i

   allocate(c1,source=b2)
   b1=>b2
   call sub(merge(c1,b1,.true.))
   call sub(merge(c1,b2,.true.) )
   c1=>i
   call sub(merge(c1,i,.true.) )
   contains
      subroutine sub(dt)
          class(*),intent(in) :: dt
      end subroutine
end

