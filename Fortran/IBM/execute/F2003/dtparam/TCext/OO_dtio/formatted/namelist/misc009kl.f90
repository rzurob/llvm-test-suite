!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : misc009kl
!*
!*  PROGRAMMER                 : David Forster (derived from misc009 by Robert Ma)
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
!*  DESCRIPTION                : Testing: Select type: printing out associate-name components
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base (lb)
      integer, len :: lb
      character(lb) :: c = ''
   end type
   type, extends(base) :: child (lc)
      integer, len :: lc
      character(lc) :: g = ''
   end type
end module

program misc009kl
   use m1
        
   class(base(:)), allocatable  :: dummy(:) ! tcx: (:)
   allocate( dummy(1), source = (/ child(3,3)('abc','def') /) ) ! tcx: (3,3)

   select type( dummy )
      type is (child(*,*)) ! tcx: (*,*)
         print *, dummy%c
         print *, dummy%g
   end select

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 1 changes
! type: child - added parameters (lc) to invoke with (3,3) / declare with (*,*) - 2 changes
