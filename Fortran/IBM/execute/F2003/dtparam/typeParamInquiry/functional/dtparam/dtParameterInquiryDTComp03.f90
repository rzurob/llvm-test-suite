!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 21 2008
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
!* 3. INQUIRY TYPE PARAMETER OF DERIVED TYPE COMPONENT
!* 4. DERIVED TYPE COMPONENT HAS TYPE PARAMETER
!* 5. THE TYPE PARAMETER OF DERIVED TYPE COMPONENT IS DEFFERED
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   type :: A(l1)
     integer(2),len  :: l1
     character(:),pointer :: c1=>null()
   end type

   type :: B(l2)
      integer,len :: l2
      character(:),allocatable :: c2
      type(A(:)),allocatable :: a1
      type(A(:)),pointer     :: a2=>null()
   end type

end module

  program dtParameterInquiryDTComp03
  use m
  implicit none

  type(B(:)),allocatable :: b1
  type(B(:)),pointer     :: b2=>null()

  allocate(B(2) :: b1)
  allocate(B(3) :: b2)


  b1%c2='xlftest'

  allocate(A(b1%l2+1) :: b1%a1)
  allocate(b1%a1%c1,source="12345")

  allocate(A(b1%l2-1) :: b1%a2)
  allocate(b1%a2%c1,source="67890"(1:3))

  if(b1%l2 /= 2)                                            error stop 10_4
  if(b1%l2%kind /= 4)                                       error stop 11_4
  if(b1%c2%len /= 7)                                        error stop 12_4
  if(b1%c2 /= "xlftest")                                    error stop 13_4
  if(b1%a1%l1 /= 3)                                         error stop 14_4
  if(b1%a1%c1%len /= 5)                                     error stop 15_4
  if(b1%a1%c1 /= '12345')                                   error stop 16_4
  if(b1%a2%l1 /= 1)                                         error stop 17_4
  if(b1%a2%c1%len /= 3)                                     error stop 18_4
  if(b1%a2%c1 /= "678")                                     error stop 19_4

  b2%c2=b1%c2(1:3)
  b2%a1=A(4)(c1=b1%a1%c1(2:4))
  allocate(b2%a2,source=A(1)(c1=b1%a1%c1))

  if(b2%l2 /= 3)                                            error stop 20_4
  if(b2%l2%kind /= 4)                                       error stop 21_4
  if(b2%c2%len /= 3)                                        error stop 22_4
  if(b2%c2 /= "xlf")                                        error stop 23_4
  if(b2%a1%l1 /= 4)                                         error stop 24_4
  if(b2%a1%c1%len /= 3)                                     error stop 25_4
  if(b2%a1%c1 /= '234')                                     error stop 26_4
  if(b2%a2%l1 /= 1)                                         error stop 27_4
  if(b2%a2%c1%len /= 5)                                     error stop 28_4
  if(b2%a2%c1 /= "12345")                                   error stop 29_4
end
