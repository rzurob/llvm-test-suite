!*  ===================================================================
!*
!*  TEST CASE NAME             : C460_002dkl
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C460 Each binding-name in binding-name-list
!*                                                  shall be the name of a specific binding of the type.
!*
!*                                             - binding is final binding
!*                               adaptation: exposed kind, length
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

   type base1 (lbase1_1) ! lbase1_1=3
      integer, len :: lbase1_1
      character(lbase1_1) :: c
      contains
         generic :: write(unformatted) => write
         final :: write
   end type

   type base2 (kbase2_1) ! kbase2_1=4
      integer, kind :: kbase2_1
      integer(kbase2_1) :: c
      contains
         generic, private :: read(formatted) => read
         final :: read
   end type

   contains

      subroutine write (dtv)
         type(base1(*)), intent(inout) :: dtv ! tcx: (*)
      end subroutine
      subroutine read (dtv)
         type(base2(4)), intent(inout) :: dtv ! tcx: (4)
      end subroutine

end module

program C460_002dkl
end program


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (lbase1_1) to invoke with (3) / declare with (*) - 1 changes
! type: base2 - added parameters (kbase2_1) to invoke with (4) / declare with (4) - 1 changes
