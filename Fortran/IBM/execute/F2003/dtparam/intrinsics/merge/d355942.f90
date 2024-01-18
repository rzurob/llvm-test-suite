!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d355942.f
!*
!*  DATE                       : Sept. 10 2008
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
!* 3. DEFECT 355942
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type A(l)
     integer(4),len  :: l=4
  end type
  contains
     function getDT1(dt)
        type(A(*)),intent(in) :: dt
        type(A(:)),allocatable :: getDT1
        print *,"in getDT1:",dt%l
        getDT1=dt
     end function

     function getDT2(dt)
        type(A(*)),intent(in) :: dt
        type(A(:)),allocatable :: getDT2

        print *,"in getDT2:",dt%l
        getDT2=merge(dt,dt,.true.)
     end function
end module

program d355942
   use m
   implicit none

   type(A(4)) :: a1
   type(A(:)),allocatable :: a2

   a2=getDT1(a1)
   print *,a2%l
   a2=getDT2(a1)
   print *,a2%l

end
