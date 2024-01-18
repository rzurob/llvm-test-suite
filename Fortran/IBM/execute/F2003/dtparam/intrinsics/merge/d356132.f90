!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d356132.f
!*
!*  DATE                       : Sept. 15 2008
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
!* 3. DEFECT 356132
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(k1,l1)
     integer,kind :: k1=2
     integer,len  :: l1=3
  end type
end module

program d356132
   use m
   implicit none

   type(A),target           :: a1
   type(A(2,:)),allocatable :: a2
   type(A(2,:)),pointer     :: a3

   a2=A(2,3)()
   a3=>a1
   print *,a2%k1,a2%l1
   print *,a3%k1,a3%l1
   call check1(merge(a2,A(2,3)(),.true.))
   call check1(merge(a3,A(2,3)(),.true.))
   call check1(merge(A(2,3)(),a3,.true.))
   contains
     subroutine check1(dt)
         type(A(2,*)) :: dt
          print *,dt%k1,dt%l1
     end subroutine

end program

