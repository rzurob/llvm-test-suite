!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : specific002akl
!*
!*  PROGRAMMER                 : David Forster (derived from specific002a by Robert Ma)
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
!*                                    - deferred specific type bound procedure (with Array)
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
         procedure(winf), deferred, pass :: write
         procedure(rinf), deferred, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read  => readchild
   end type

   interface
      subroutine winf (dtv, unit, iotype, v_list, iostat, iomsg)
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
      subroutine rinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtiowrite'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioread'

      end subroutine

end module

program specific002akl
   use m

   integer(4) :: stat
   character(200) :: msg


   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:) ! tcx: (:)

   type(child(3,4)), target      :: c1(2) ! tcx: (3,4)
   class(child(:,4)), pointer    :: c2(:) ! tcx: (:,4)

   namelist /n1/ b1, c1
   namelist /n2/ b2, c2

   allocate ( b1(3), source = (/ child(3,4) ( 'abc', 1001 ), child(3,4) ( 'def', 1002 ), child(3,4) ( 'ghi', 1003 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( b2(3), source = (/ child(3,4) ( 'ABC', 2001 ), child(3,4) ( 'DEF', 2002 ), child(3,4) ( 'GHI', 2003 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   c1= (/ child(3,4) ( 'jkl', 3003 ), child(3,4) ( 'mno', 3004 ) /) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( c2(2), source = (/ child(3,4) ( 'JKL', 4003 ), child(3,4) ( 'MNO', 4004 ) /) ) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'specific002akl.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, b2, c2 )
   allocate ( child(3,4) :: b1(3), b2(3), c2(2) ) ! tcx: (3,4)
   c1 = (/ child(3,4)(), child(3,4)() /) ! tcx: (3,4) ! tcx: (3,4)

   read ( 1, n1, iostat = stat, iomsg = msg )
   print *, stat, msg
   print *, b1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read ( 1, n2, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%i /= 1001 ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%i /= 1002 ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%i /= 1003 )      ) error stop 5_4
   end select

   select type ( b2 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b2(1)%c /= 'ABC' ) .or. (b2(1)%i /= 2001 ) .or. &
              ( b2(2)%c /= 'DEF' ) .or. (b2(2)%i /= 2002 ) .or. &
              ( b2(3)%c /= 'GHI' ) .or. (b2(3)%i /= 2003 )      ) error stop 6_4
   end select

   if ( ( c1(1)%c /= 'jkl' ) .or. ( c1(1)%i /= 3003 ) .or. &
        ( c1(2)%c /= 'mno' ) .or. ( c1(2)%i /= 3004 )      ) error stop 7_4
   if ( ( c2(1)%c /= 'JKL' ) .or. ( c2(1)%i /= 4003 ) .or. &
        ( c2(2)%c /= 'MNO' ) .or. ( c2(2)%i /= 4004 )      ) error stop 8_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 19 changes
