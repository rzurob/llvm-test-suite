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
! %POSTCMD: dcomp funcSub002.f
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
!*                                        Return abstract type array object
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

   type, extends(base) :: child(k3)
      integer, kind :: k3
   contains
      procedure, nopass :: foo
   end type

   interface
      type(base(4)) function fooif()
         import base
         dimension :: fooif(0)
      end function
   end interface

contains

   type(base(4)) function foo()
      dimension :: foo(0)
      foo = (/ (base(1),i=1,0) /)
   end function

   function foo1() result(myfoo)
      type(base(4)), pointer :: myfoo(:)
      myfoo => null()
   end function

end module

program funcSub002

end program

