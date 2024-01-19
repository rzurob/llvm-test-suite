!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Dummy argument being derived type component and bind(C) procedure pointer
!*                                        and function return (specified by result=) is procedure pointer
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

   use ISO_C_BINDING

   interface
      subroutine seti1(i1, i2) bind(C)
         import C_INT
         integer(C_INT), intent(out) :: i1
         integer(C_INT), intent(in)  :: i2
      end subroutine
   end interface

   type base
      integer(C_INT) :: i1
      procedure(seti1), pointer, nopass :: pp
   end type

   contains

      function getpp(pp1) result(mypp)
         procedure(seti1), pointer,  intent(in) :: pp1
         procedure(seti1), pointer :: mypp

         mypp => pp1

      end function

end module

program bindc024
   use m

   procedure(seti1), pointer :: pp1 => null()
   type(base), allocatable :: b1

   pp1 => seti1
   allocate ( b1 )

   b1%pp => getpp(pp1)

   call b1%pp(b1%i1, 101)

   if ( b1%i1 /= 101 ) error stop 1_4

end program
