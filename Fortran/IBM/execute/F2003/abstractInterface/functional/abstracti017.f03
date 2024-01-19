! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: the derived-type-spec shall not specify an ABSTRACT type (C401)
!*                                        SelectType Construct: type-guard-stmt CLASS IS specifies type-spec is Abstract type
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2
   contains
      procedure, nopass :: print
   end type

   abstract interface
      subroutine printif()
      end subroutine
   end interface
contains
   subroutine print()
      print *,'b2'
   end subroutine
end module

program abstracti017
   use m
   class(b1), allocatable :: b11
   allocate(b2 :: b11)

   select type ( b => b11 )
      class is ( b1 )
         call b%print()
   end select

end program abstracti017
