!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355924.f
!*
!*  DATE                       : Sept. 9 2008
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
!* 3. DEFECT 355924
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer(8),len  :: l=4
     character(2*l),pointer :: c4=>null()
  end type
end module

program d355924
   use m
   implicit none

   type(A(4)):: a1
   type(A(:)),allocatable :: a2

   print *,"before call merge"
   print *,a1%l,a1%c4%len,len(a1%c4)

   a2=merge(a1,a1,.true.)
   print *,"after call merge"
   print *,a2%l,a2%c4%len,len(a2%c4)

   print *,"before assignment"
   print *,a1%l,a1%c4%len,len(a1%c4)

   a2=a1
   print *,"after assignment"
   print *,a2%l,a2%c4%len,len(a2%c4)

end

