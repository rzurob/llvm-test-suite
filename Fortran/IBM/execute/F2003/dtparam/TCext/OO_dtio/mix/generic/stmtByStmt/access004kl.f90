!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : access004kl
!*
!*  PROGRAMMER                 : David Forster (derived from access004 by Robert Ma)
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
!*                               Stmt by Stmt: (pg.58 ln3-6) The default accessibility for the procedure bindings
!*                                             of a type is private if the type definition contains
!*                                             a binding-private-stmt, and public otherwise. The
!*                                             accessibility of a procedure binding may be explicitly
!*                                             declared by an access-spec; otherwise its accessibility
!*                                             is the default for the type definition in which it is
!*                                             declared.
!*
!*                                             - Even specific binding has private attr, generic binding should be able to be called
!*                               adaptation: exposed length
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

   type :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         private
         procedure, pass :: write => writebase
         procedure, private, pass :: read => readbase
         generic, public :: write(formatted) => write
         generic, public :: read(formatted)  => read
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

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadbase'

      end subroutine

end module

program access004kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: c1 ! tcx: (:)
   type(base(3))               :: c2 = base(3) ( 'abc' ) ! tcx: (3) ! tcx: (3)

   integer :: stat
   character(200) :: msg

   namelist /n1/ b1, c1, c2


   allocate ( b1, source = base(3) ( 'ibm' ) ) ! tcx: (3)
   allocate ( c1, source = base(3) ( 'ftn' ) ) ! tcx: (3)

   open ( 101, file = 'access004kl.1', form='formatted', access='sequential' )

   write ( 101, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritebase' ) ) error stop 101_4

   rewind 101

   b1%c = 'xxx'
   c1%c = 'xxx'
   c2%c = 'xxx'

   read ( 101, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadbase' ) ) error stop 2_4
   if ( ( c2%c /= 'abc' ) .or. ( b1%c /= 'ibm' ) .or. ( c1%c /= 'ftn' ) ) error stop 3_4

   close (101, status = 'delete' )


end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
