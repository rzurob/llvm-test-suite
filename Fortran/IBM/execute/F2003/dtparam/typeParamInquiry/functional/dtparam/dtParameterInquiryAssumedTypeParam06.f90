!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryAssumedTypeParam06.f
!*
!*  DATE                       : July 18 2008
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
!* 3. BASIC ASSUMED LENGTH PARAMETER TEST
!* 4. DIFFERENT ACTAUL ARGUMENT
!* 5. DEFECT 354105
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
      integer(8),len :: l
   end type

   contains
      subroutine check1(b,n)
        type(base(*)) :: b
        integer :: n
        print *,"TEST ",n
        print *,b%l,b%l%kind,kind(b%l)
      end subroutine

      subroutine check2(b,n)
        class(base(*)) :: b
        integer :: n
        select type(a=>b)
           type is(base(*))
             print *,"TEST ",n
             print *,a%l,a%l%kind,kind(a%l)
           class default
             error stop 10_4
        end select
      end subroutine

      subroutine check3(b,n)
        class(*) :: b
        integer :: n
        select type(a=>b)
           type is(base(*))
             print *,"TEST ",n
             print *,a%l,a%l%kind,kind(a%l)
           class default
             error stop 11_4
        end select
      end subroutine

      class(base(:)) function getDT(b)
        class(base(*)) :: b
        pointer :: getDT
        select type(a=>b)
           type is(base(*))
              allocate(getDT,source=b)
           class default
             error stop 12_4
        end select
      end function

end module

  program dtParameterInquiryAssumedTypeParam06
  use m
  implicit none

  integer,parameter :: i1=2

  type(base(2))  :: b1
  type(base(i1)) :: b2
  type(base(:)),allocatable :: b3
  type(base(:)),pointer :: b4
  class(base(:)),allocatable :: b5
  class(base(:)),pointer   :: b6
  class(*),allocatable :: b7
  class(*),pointer    :: b8

  allocate(b3,source=b1)
  allocate(base(2) ::b4)
  allocate(b5,source=b2)
  allocate(b6,source=b1)

  call check1(b1,1)
  call check1(b2,2)
  call check1(b3,3)
  call check1(b4,4)
  call check1(b5,5)
  call check1(b6,6)

  call check2(b1,7)
  call check2(b2,8)
  call check2(b3,9)
  call check2(b4,10)
  call check2(b5,11)
  call check2(b6,12)

  call check1(getDT(b1),13)
  call check1(getDT(b2),14)
  call check1(getDT(b3),15)
  call check1(getDT(b4),16)
  call check1(getDT(b5),17)
  call check1(getDT(b6),18)

  allocate(base(2) :: b7)
  call check3(b7,19)
  allocate(base(2) :: b8)
  call check3(b8,20)


end
