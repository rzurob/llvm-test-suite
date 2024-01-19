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
!* 2. TYPE PARAMETER INQUIRY
!* 3. INTRINSIC ASSIGNMENT
!* 4. COMPONENT IS DEFERRED DERIVED TYPE
!* 5. DEFECT 355338, 355327
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type B(lb)
      integer,len :: lb
   end type
   type A(la)
      integer,len   :: la
      type(B(:)),allocatable :: b1
      type(B(:)),pointer     :: b2
   end type
end module

program dtParameterInquiryDTAssign04
  use m
  implicit none

  type(A(3)),target :: a1
  type(A(:)),allocatable :: a2
  type(A(:)),allocatable :: a3
  type(A(:)),pointer     :: a4=>null()
  type(A(:)),pointer     :: a5=>null()

  a1%b1=B(6)()
  allocate(B(2*a1%b1%lb) :: a1%b2)

  if(a1%la /= 3)                                      error stop 10_4
  if(a1%b1%lb /= 6)                                   error stop 11_4
  if(a1%b2%lb /= 12)                                  error stop 12_4

  a2=a1
  if(a2%la /= 3)                                      error stop 13_4
  !--- defect 355338--!
  if(a2%b1%lb /= 6)                                   error stop 14_4
  if(a2%b2%lb /= 12)                                  error stop 15_4

  a3=a2
  !--- defect 355327---!
  if(a3%la /= 3)                                      error stop 16_4
  if(a3%b1%lb /= 6)                                   error stop 17_4
  if(a3%b2%lb /= 12)                                  error stop 18_4

  a4=>a1

  if(a4%la /= 3)                                      error stop 19_4
  if(a4%b1%lb /= 6)                                   error stop 20_4
  if(a4%b2%lb /= 12)                                  error stop 21_4

  a5=>a1

  if(a5%la /= 3)                                      error stop 22_4
  if(a5%b1%lb /= 6)                                   error stop 23_4
  if(a5%b2%lb /= 12)                                  error stop 24_4

end
