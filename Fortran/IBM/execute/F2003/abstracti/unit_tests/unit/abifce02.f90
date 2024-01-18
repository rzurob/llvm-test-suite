!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Abstractr Interface
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Functional test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : When the type-bound procedure does not have
!*                               a default or natural implementation, but 
!*                               rather only a well-defined purpose and 
!*                               interface. An abstract interface can be
!*                               used for the bound procedure.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module m
type, abstract :: base
     contains
     procedure (absinter), pass(person), deferred :: proc
end type

type, extends (base) :: child
   character*10 name
   contains
   procedure :: proc => childProc
end type


abstract interface
   subroutine absinter (person)
      import base
      class (base), intent (in) :: person
   end subroutine
end interface

contains 

subroutine childProc (person)
   class (child), intent(in) :: person
   print *, person%name
end subroutine

end module                   

program main
use m
class( base),pointer:: a
allocate (a, source = child("Jonnason"))
call a%proc
end program
