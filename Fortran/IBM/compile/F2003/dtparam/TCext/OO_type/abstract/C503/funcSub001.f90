!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp funcSub001.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), function return cannot be abstract type, type(abstract type)
!*                                        Return Scalar abstract type object, pointer
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(fooif), nopass, deferred :: foo
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
   contains
      procedure, nopass :: foo
   end type

   interface
      type(base(4)) function fooif()
         import base
      end function
   end interface

contains

   type(base(4)) function foo()
      foo = base(4)(4)
   end function

   function boo() result(boo1)
      type(base(4)), pointer :: boo1
      boo1 => null()
   end function

end module

program funcSub001

end program

