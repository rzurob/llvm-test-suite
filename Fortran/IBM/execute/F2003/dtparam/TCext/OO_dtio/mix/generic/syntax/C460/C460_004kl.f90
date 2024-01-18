!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C460_004kl
!*
!*  PROGRAMMER                 : David Forster (derived from C460_004 by Robert Ma)
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Syntax Check: C460 Each binding-name in binding-name-list
!*                                                  shall be the name of a specific binding of the type.
!*
!*                                             - binding is inherited from base type but is overridden by child's binding
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
         procedure, pass :: write => writebase
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i
      contains
         procedure, pass :: write => writechild
         generic :: write(formatted) => write
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowritebase'

      end subroutine


      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,I5)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritechild'

      end subroutine

end module

program C460_004kl
   use m

   class(child(:,4)), allocatable :: b1 ! tcx: (:,4)
   class(child(:,4)), pointer     :: c1 ! tcx: (:,4)
   type(child(3,4))               :: c2 = child(3,4) ( 'abc', 1234 ) ! tcx: (3,4) ! tcx: (3,4)
   integer :: stat
   character(200) :: msg

   namelist /nml/ b1

   allocate ( b1, source = child(3,4) ( 'ibm', 2005 ) ) ! tcx: (3,4)
   allocate ( c1, source = child(3,4) ( 'ftn', 2003 ) ) ! tcx: (3,4)

   open ( 101, file = 'C460_004kl.1', form='formatted', access='sequential' )

   write ( 101, nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) ) error stop 1_4
   msg = ''
   write ( 101, *, iostat = stat, iomsg = msg )   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) ) error stop 2_4
   msg = ''
   write ( 101, "(DT)", iostat = stat, iomsg = msg )   c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) ) error stop 3_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 1 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 7 changes
