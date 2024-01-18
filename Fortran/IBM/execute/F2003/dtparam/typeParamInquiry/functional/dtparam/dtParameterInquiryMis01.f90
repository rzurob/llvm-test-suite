!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryMis01.f
!*
!*  DATE                       : July 26 2008
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
!* 3. TYPE PARAMETER INQUIRY AS ACTUAL ARGUMENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(k,l)
       integer(2),kind :: k
       integer(8),len :: l
       character(:),allocatable :: c(:)
    end type
end module

  program dtParameterInquiryMis01
  use m
  implicit none

  type(base(3,6)) :: b1
  character(:),pointer ::p1

  if(b1%k /= 3)                                            error stop 10_4
  if(b1%l /= 6)                                            error stop 11_4

  allocate(character(len=2*b1%l) :: b1%c(b1%k:b1%l))

  if(b1%c%len /= len(b1%c) .or. b1%c%len /= 12)            error stop 12_4

  allocate(p1,source=getchar(2*b1%l))
  if(p1%len /= len(p1) .or. p1%len /= 12)                  error stop 13_4
  if(p1 /='this is test')                                  error stop 14_4

  deallocate(p1)
  allocate(p1,source=getchar(b1%k+b1%l))
  if(p1%len /= len(p1) .or. p1%len /= 9)                   error stop 15_4
  if(p1 /='this is t')                                     error stop 16_4

  associate(x=>gettotalparam(b1%k,b1%l,b1%c%len,lbound(b1%c,1),ubound(b1%c,1)))
     if(x /= 30)                                           error stop 17_4
  end associate

  contains

     character(:) function getchar(length)
        integer(8) :: length
        pointer :: getchar
        allocate(character(length) :: getchar)
        getchar="this is test"
     end function

     integer function gettotalparam(k,l,c_len,c_lbd,c_ubd)
        integer(2) :: k
        integer(8) :: l
        integer :: c_len,c_lbd,c_ubd
        gettotalparam=k+l+c_len+c_lbd+c_ubd
     end function
end

