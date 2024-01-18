!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d341446
!*
!*  DATE                       : 2007-09-14
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: Run-Time SELECT TYPE Skips Type Guard
!*                               with Kind Type Parameter
!*
!*  DESCRIPTION                :
!*  The Reduced Code below Fails at Run-Time when the Type Guard Statement
!*  with a Kind Type Parameter is not selected.
!*
!*  NOTE:  Defect 339937 describes a similar failure that involves Length
!*  Type Parameters.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=1
      integer, kind :: kchild_1
      character(kchild_1,3) :: c
   end type

   class(base), pointer     :: b2

   contains

   subroutine initialize ()
      allocate ( b2, source = child(1)('IBM') ) ! tcx: (1)
   end subroutine

end module

program d341446
use m

   call initialize()

   select type ( c => b2 )
      class is (child(1)) ! tcx: (1)

      class default
         stop 11
   end select

end program d341446
