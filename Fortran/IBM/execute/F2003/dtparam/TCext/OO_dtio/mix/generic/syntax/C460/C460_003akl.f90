!*  ===================================================================
!*
!*  TEST CASE NAME             : C460_003akl
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
!*                                             - binding is inherited from base type, ensure type base cannot use generic TB
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c
      contains
         procedure, pass :: write => writechild
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i
      contains
         generic :: write(formatted) => write
   end type

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowritechild'

      end subroutine

end module

program C460_003akl
   use m

   integer :: i = 0
   character(150) :: c = ''

   write(6, *, iostat = i, iomsg = c )  base(3) ('IBM') ! tcx: (3)
   if ( ( i /= 0 ) .or. ( c /= "" ) ) error stop 101_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 2 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 0 changes
