!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. DEFECT 355338
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B(lb)
      integer,len :: lb
   end type
   type A(la)
      integer,len   :: la
      type(B(:)),allocatable :: b1
   end type
end module

program d355338
  use m
  implicit none

  type(A(3)) :: a1
  type(A(:)),allocatable :: a2

  a1%b1=B(6)()

  print *,a1%la,a1%b1%lb
  a2=a1
  print *,a2%la,a2%b1%lb
  if(allocated(a2))  deallocate(a2)
  allocate(a2,source=a1)
  print *,a2%la,a2%b1%lb

end
