!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 24 2008
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
!* 2. TYPE PARAMETER IS NOT VARIABLE,CAN NEVER BE ASSIGNED TO
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k,l)
      integer(2),kind :: k
      integer(8),len  :: l
      character(l)    :: c
   end type
end module

  program dtParameterInquiryDiag01
  use m
  implicit none

  integer(2) :: i1
  integer(kind(2)) :: i2
  character(kind=1,len=len('abc')) :: c1
  character(:),allocatable :: c2
  character(:),pointer :: c3
  type(base(2,4)) :: t1
  type(base(2,:)),allocatable :: t2
  type(base(2,:)),pointer :: t3

  i1%kind=2
  i2%kind=2
  c1%kind=1
  c1%len=3
  c2%len=1
  c3%len=2
  t1%k=2
  t1%l=4
  t1%k%kind=2
  t1%l%kind=8
  t1%c%kind=1
  t1%c%len=t1%l
  t2%l=2
  t2%l%kind=8
  t2%c%kind=t2%k
  t2%c%len=t2%l
  t3%l=2
  t3%l%kind=kind(t3%l)
  t3%c%len=len(t3%c)


  end
