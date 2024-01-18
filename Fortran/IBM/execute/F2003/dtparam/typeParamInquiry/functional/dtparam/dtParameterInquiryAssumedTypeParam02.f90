!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 17 2008
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
!* 3. USE ASSUMED LENGTH PARAMETER
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type ::base(l1,l2)
      integer(2),len  :: l1
      integer(4),len  :: l2
      character(:),allocatable ::c1(:,:)
      character(3) :: c2(l1,l2)
   end type
end module

  program dtParameterInquiryAssumedTypeParam02
  use m
  implicit none

  type(base(2,3)) :: b1
  character(:),allocatable :: c1(:,:)


  call sub1(b1)
  if(ubound(c1,1) /= 2)                                    error stop 10_4
  if(ubound(c1,2) /= 3)                                    error stop 11_4
  if(any(c1 /= "xlftest" ))                                error stop 12_4
  if(ubound(b1%c1,1) /= 1)                                 error stop 13_4
  if(ubound(b1%c1,2) /= 5)                                 error stop 14_4
  if(ubound(b1%c2,1) /= 2)                                 error stop 15_4
  if(ubound(b1%c2,2) /= 3)                                 error stop 16_4
  if(any(b1%c1 /= "xlftest" ))                             error stop 17_4

  deallocate(c1,b1%c1)


  contains
    subroutine sub1(t)
       type(base(l1=*,l2=*)) :: t
       allocate(c1(t%l1,t%l2),source="xlftest")
       allocate(b1%c1(t%l2-t%l1,t%l1+t%l2),source=c1(1,1))
       if(ubound(t%c2,1) /= t%l1 .or. ubound(t%c2,2) /= t%l2)  error stop 18_4
    end subroutine

end
