
!*  ===================================================================
!*
!*  TEST CASE NAME             : move_allocProcPtrComp01.f
!*
!*  DATE                       : Oct. 8 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM,TO)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.82
!*  2. DERIVED TYPE HAS PROCEDURE POINTER COMPONENT WHICH POINTS TO SUBROUTINE
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type B(l1)
     integer,len   :: l1
     integer       :: i1(l1)
     procedure(),nopass,pointer :: procptr=>null()
  end type
  contains
     subroutine sub()
         print *,"in sub"
     end subroutine
end module

program move_allocProcPtrComp01

  use m
  implicit none

  type(B(:)),allocatable   :: from1,to1

  allocate(from1,source= B(2)([4,5]))

  from1%procptr=>sub

  call from1%procptr

  call move_alloc(from1,to1)

  if(allocated(from1))                        error stop 10_4
  if(.not. allocated(to1))                    error stop 11_4

  if(any(to1%i1 /= [4,5]))                    error stop 12_4
  if(.not. associated(to1%procptr,sub))       error stop 13_4

  call to1%procptr
end program

