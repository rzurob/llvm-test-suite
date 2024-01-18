!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : misc011kl
!*
!*  PROGRAMMER                 : David Forster (derived from misc011 by Robert Ma)
!*  DATE                       : 2007-07-06 (original: 21/03/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: OO intrinsics: data-ref with > 2 part-ref (defect 300649)
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

   type :: base (lb)
      integer, len :: lb
      class(base(:)), pointer :: next => null()
      character(lb) :: c = 'xxx'
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i = -999
   end type

   type :: linkedlist
      class(base(:)), pointer :: head ! tcx: (:)
   end type

end module

program misc011kl
   use m
   class(linkedlist), pointer :: ll
   class(base(:)), pointer :: dummy ! tcx: (:)

   class(base(:)), allocatable, target :: b1, b2 ! tcx: (:)

   allocate ( b1, source = base(3)(c='IBM') ) ! tcx: (3)
   allocate ( b2, source = child(3,4)(c='FTN', i=1000) ) ! tcx: (3,4)

   allocate ( ll )

   ll%head => b1
   b1%next => b2

   if ( .not. same_type_as ( ll%head%next, b2 ) )    error stop 1_4
   if ( .not. extends_type_of ( ll%head%next, b2 ) ) error stop 2_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 1 changes
