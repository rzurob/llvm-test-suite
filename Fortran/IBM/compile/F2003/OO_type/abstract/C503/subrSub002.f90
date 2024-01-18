! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Subroutine subprogram (Section 12.5.2.1)
!*                               dummy-arg being non-polymorphic abstract type
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: foo
   end type

contains

   subroutine foo(dtv)
      type(base), intent(in) :: dtv
      print *,"error"
   end subroutine

   recursive subroutine boo(dtv)
      type(base), intent(out) :: dtv
      print *,"error"
   end subroutine

end module


program subrSub002

end program