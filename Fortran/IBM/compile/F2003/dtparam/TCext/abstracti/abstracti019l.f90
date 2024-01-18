!***********************************************************************
!* =====================================================================
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-25 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1)
!*  function return cannot be abstract type, type(abstract type) Return
!*  Scalar abstract type object, pointer
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!* =====================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base (lbase_1) ! lbase_1=8
      integer, len :: lbase_1
      integer(4) :: id
   contains
      procedure(fooif), nopass, deferred :: foo
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: foo
   end type

   abstract interface
      type(base(8)) function fooif() ! tcx: (8)
         import base
      end function
   end interface

contains

   type(base(8)) function foo() ! tcx: (8)
      foo = base(8)(4) ! tcx: (8)
   end function

   function boo() result(boo1)
      type(base(8)), pointer :: boo1 ! tcx: (8)
      boo1 => null()
   end function

end module

program abstracti019l

end program abstracti019l



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (8) / declare with (8) - 4 changes
