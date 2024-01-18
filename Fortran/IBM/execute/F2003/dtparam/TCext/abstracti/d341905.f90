!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d341905
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti004l)
!*
!*  DATE                       : 2007-09-26
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DEFECT ABSTRACT            : DTPARAM: Run-Time SIGSEGV: Program Jumps
!*                               to Arbitrary Entry Point
!*
!*  DESCRIPTION                :
!*  When executed, this program will coredump with a Segmentation violation
!*  in the FUNCTION "getc()" during a call to the SUBROUTINE "setc()".
!*
!*  NOTE(s):
!*  1)  The FUNCTION "getc()" in the Reduced Code below is dead code.
!*  2)  Extensions from the Original Test Case are noted below.
!*  3)  The Original Test Case may be found through the following path:
!*
!*      /tstdev/F2003/abstracti/functional/abstracti004.scenario
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type, abstract :: base (lchild) ! lchild=3
      integer, len :: lchild
      contains
      procedure(inf1), pass, deferred :: getc
      procedure(inf2), pass, deferred :: setc
   end type

   type, extends(base) :: child
      character(lchild), private :: c = 'xxx'
      contains
      procedure, pass :: getc
      procedure, pass :: setc
   end type

   abstract interface
      character(3) function inf1(dtv)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
      end function
   end interface

   abstract interface
      subroutine inf2(dtv,c)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         character(3), intent(in) :: c
      end subroutine
   end interface

   class(base(:)), allocatable :: b1 ! tcx: (:)

   contains

   subroutine initialize ()
      print *, "initialize()"
      allocate ( b1, source = child(3)('xxx') ) ! tcx: (3)
   end subroutine

   character(3) function getc(dtv)
      class(child(*)), intent(in) :: dtv ! tcx: (*)
      getc =dtv%c
   end function

   subroutine setc(dtv,c)
      class(child(*)), intent(inout) :: dtv ! tcx: (*)
      character(3), intent(in) :: c
      dtv%c = c
   end subroutine

end module

program d341905
use m

   print *, "call initialize()"
   call initialize()

   print *, "call b1%setc()"
   call b1%setc('abc')

end program d341905
