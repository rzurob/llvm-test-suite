! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure declaration statement
!*                                        external functions (with implicit interface)
!*                                        with return type which is non-polymorphic abstract type
!*
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
   integer i
end type

type, extends(base) :: child
end type

end module

program procDeclrStmt001
   use m

   class(base), pointer :: b1
   procedure(type(base)) :: genbaseptr
   allocate(b1, source=genbaseptr())
   print *,b1%i
   call printbase( genbaseptr() )

contains

   subroutine printbase(a)
      use m
      class(base), intent(in) :: a
      print *,a%i
   end subroutine

end program

type(base) function genbaseptr()
   use m
   genbaseptr=base(5)
end function

