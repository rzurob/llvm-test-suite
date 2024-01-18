!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : specific009lk
!*
!*  PROGRAMMER                 : David Forster (derived from specific009 by Robert Ma)
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
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
!*                                 - Specific Binding
!*                                    - deferred specific type bound procedure
!*                                         - deferred binding in two parent types, and implemented in gen3 type
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

   type, abstract :: base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure(wbinf), deferred, pass :: write
         procedure(rbinf), deferred, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, abstract, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure(wcinf), deferred, pass :: write
         procedure(rcinf), deferred, pass :: read
   end type

   type, extends(child) :: gen3
      integer(kchild_1) :: j = -999
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read  => readg
   end type

   interface
      subroutine wbinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine rbinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine wcinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine rcinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg)   dtv%c, dtv%i, dtv%j
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         iomsg = 'dtioreadg'

      end subroutine

end module

program specific009lk
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable  :: b1 ! tcx: (:)
   class(child(:,4)), pointer     :: c1 ! tcx: (:,4)
   class(gen3(:,4)), pointer      :: g1 ! tcx: (:,4)

   type(gen3(3,4))                :: g2 ! tcx: (3,4)
   type(gen3(3,4)), parameter     :: g3 = gen3(3,4)('mno',501,502) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'specific009lk.1', form='formatted', access='sequential' )

   allocate ( b1, source = gen3(3,4)('abc',101, 102)) ! tcx: (3,4)
   allocate ( c1, source = gen3(3,4)('def',201, 202)) ! tcx: (3,4)
  allocate ( g1, source = gen3(3,4)('ghi',301, 302)) ! tcx: (3,4)

   g2 = gen3(3,4)('jkl',401,402) ! tcx: (3,4)

   write ( 1, *, iostat = stat, iomsg = msg )  b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 1_4

   write ( 1, *, iostat = stat, iomsg = msg )  g2, g3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, c1, g1 )
   allocate ( gen3(3,4) :: b1, c1, g1 ) ! tcx: (3,4)

   read ( 1, *, iostat = stat, iomsg = msg )  b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) ) error stop 3_4

   read ( 1, *, iostat = stat, iomsg = msg )  g2, g2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) ) error stop 4_4

   select type ( b1 )
      type is ( gen3(*,4) ) ! tcx: (*,4)
         select type ( c1 )
            type is ( gen3(*,4) ) ! tcx: (*,4)
               if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 101 ) .or. ( b1%j /= 102 ) .or. &
                    ( c1%c /= 'def' ) .or. ( c1%i /= 201 ) .or. ( c1%j /= 202 ) .or. &
                    ( g1%c /= 'ghi' ) .or. ( g1%i /= 301 ) .or. ( g1%j /= 302 ) .or. &
                    ( g2%c /= 'mno' ) .or. ( g2%i /= 501 ) .or. ( g2%j /= 502 ) ) error stop 5_4
         end select
   end select

   close (1, status = 'delete' )

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 3 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 3 changes
! type: gen3 - added parameters () to invoke with (3,4) / declare with (*,4) - 13 changes
