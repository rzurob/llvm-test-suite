!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullActualArg03.f
!*
!*  DATE                       : Sept. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 13.7.88
!* 2. NULL([MOLD])
!* 3. MOLD IS POINTER OR ALLOCABLE
!* 4. NULL([MOLD]) IS PASSED THROUGH MULTIPLE SUBROUTINE
!* 5. NULL([MOLD]) IS USED AS ACTUAL ARGUMENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l)
       integer,len  :: l
       character(l) :: c1
   end type
end module

program nullActualArg03
   use m
   implicit none

   type(dtp(3)),target      :: dtp1=dtp(3)(c1="123")
   type(dtp(3)),allocatable :: dtp2
   type(dtp(3)),pointer     :: dtp3

   call sub1(dtp2)
   call sub1(null(dtp2))
   call sub1(null())

   dtp2=dtp1

   call sub1(dtp2)
   call sub1(null(dtp2))
   call sub1(null())

   call sub3(null(dtp3))
   call sub3(null())

   dtp3=>dtp1

   call sub3(dtp3)
   call sub3(null(dtp3))
   call sub3(null())

   contains
      subroutine sub1(dt)
         type(dtp(3)),allocatable :: dt

         print *,"sub1:",allocated(dt)
         call sub2(dt)
      end subroutine

      subroutine sub2(dt)
         type(dtp(3)),allocatable :: dt

         print *,"sub2:",allocated(dt)
      end subroutine

      subroutine sub3(dt)
         type(dtp(3)),pointer :: dt

         print *,"sub3:",associated(dt)
         call sub4(dt)
      end subroutine

      subroutine sub4(dt)
         type(dtp(3)),pointer :: dt

         print *,"sub4:",associated(dt)
      end subroutine

end program
