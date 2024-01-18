!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355926.f
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
!* 3. DEFECT 355926
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer(8),len  :: l=4
     character(2*l) :: c1="xlftest"
     character(:),allocatable :: c2
  end type
end module

program d355926
   use m
   implicit none

   type(A(4)) :: a1
   type(A(:)),allocatable :: a2

   a1%c2 = "Hello"

   print *,allocated(a1%c2),allocated(a2)
   print *,"|",a1%c2,"|","|"
   a2=a1
   print *,allocated(a1%c2),allocated(a2%c2)
   print *,"|",a1%c2,"|",a2%c2,"|"

   a2=merge(a1,a1,.false.)
   print *,allocated(a1%c2),allocated(a2%c2)
   print *,"|",a1%c2,"|",a2%c2,"|"

end

