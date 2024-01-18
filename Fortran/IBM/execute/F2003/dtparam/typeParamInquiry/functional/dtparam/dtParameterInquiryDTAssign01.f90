!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryDTAssign01.f
!*
!*  DATE                       : August 23 2008
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
!* 4. DEFECT 354013,355327,355334
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1,l2)
      integer(2),len  :: l1=2
      integer(8),len  :: l2=8
      character(l1)   :: c(2)
   end type
end module

  program dtParameterInquiryDTAssign01
  use m
  implicit none

  type(A(3,2)),target :: a1=A(3,2)(c=['abc','efg'])
  type(A(:,:)),allocatable :: a2
  type(A(:,:)),allocatable :: a3
  type(A(:,:)),pointer :: a4
  type(A(:,:)),pointer :: a5

  a2=a1
  if(a2%l1 /= 3)                                     error stop 10_4
  if(a2%l2 /= 2)                                     error stop 11_4
  !--- defect 354013---!
  if(a2%c%len /= len(a2%c) .or. a2%c%len /= 3)       error stop 12_4
  if(ubound(a2%c,1) /= 2)                            error stop 13_4
  !--- defect 355334---!
  if(any(a2%c /= ['abc','efg']))                     error stop 14_4
  a3=a2
   !---- defect 355327---!
  if(a3%l1 /= 3)                                     error stop 15_4
  if(a3%l2 /= 2)                                     error stop 16_4
  !--- defect 354013--!
  if(a3%c%len /= len(a3%c) .or. a3%c%len /= 3)       error stop 17_4
  if(ubound(a3%c,1) /= 2)                            error stop 18_4
  !--- defect 355334---!
  if(any(a3%c /= ['abc','efg']))                     error stop 19_4

  a4=>a1
  if(a4%l1 /= 3)                                     error stop 20_4
  if(a4%l2 /= 2)                                     error stop 21_4
  !--- defect 354013---!
  if(a4%c%len /= len(a4%c) .or. a4%c%len /= 3)       error stop 22_4
  if(ubound(a4%c,1) /= 2)                            error stop 23_4
  !--- defect 355334---!
  if(any(a4%c /= ['abc','efg']))                     error stop 24_4

  a5=>a4
  if(a5%l1 /= 3)                                     error stop 25_4
  if(a5%l2 /= 2)                                     error stop 26_4
  !--- defect 354013---!
  if(a5%c%len /= len(a5%c) .or. a5%c%len /= 3)       error stop 27_4
  if(ubound(a5%c,1) /= 2)                            error stop 28_4
  !--- defect 355334---!
  if(any(a5%c /= ['abc','efg']))                     error stop 29_4

end
