! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C503/procDeclrStmt001.f
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

type, abstract :: base(k1)    ! (4)
   integer, kind :: k1
   integer(k1)      i
end type

type, extends(base) :: child(k2,n1)    ! (4,4,20)
    integer, kind :: k2
    integer, len  :: n1
end type

end module

program procDeclrStmt001
   use m

   class(base(4)), pointer :: b1
   procedure(type(base(4))) :: genbaseptr
   allocate(b1, source=genbaseptr())
   print *,b1%i
   call printbase( genbaseptr() )

contains

   subroutine printbase(a)
      use m
      class(base(4)), intent(in) :: a
      print *,a%i
   end subroutine

end program

type(base(4)) function genbaseptr()
   use m
   genbaseptr=base(4)(5)
end function

