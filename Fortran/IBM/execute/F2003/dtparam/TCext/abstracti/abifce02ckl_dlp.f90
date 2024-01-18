! GM DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/abstracti/unit_tests/unit/abifce02.f

!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : abifce02ckl_dlp
!*
!*                               by James Ren)
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : When the type-bound procedure does not have
!*  a default or natural implementation, but
!*  rather only a well-defined purpose and
!*  interface. An abstract interface can be
!*  used for the bound procedure.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
type, abstract :: base(k1,l1)    ! (4,20)
    integer, kind :: k1
    integer, len  :: l1
     contains
     procedure (absinter), pass(person), deferred :: proc
end type

type, extends (base) :: child(k2,l2)    ! (4,20,1,10)
   integer, kind             :: k2
   integer, len              :: l2
   character(kind=k2,len=l2)    name
   contains
   procedure :: proc => childProc
end type


abstract interface
   subroutine absinter (person)
      import base
      class (base(4,*)), intent (in) :: person
   end subroutine
end interface

contains

subroutine childProc (person)
   class (child(4,*,1,*)), intent(in) :: person
   print *, person%name
end subroutine

end module

program main
use m
class( base(4,:)),pointer:: a
allocate (a, source = child(4,20,1,10)("Jonnason"))
call a%proc
end program
