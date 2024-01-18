!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar001akl
!*
!*  DATE                       : 2007-08-13 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar (non-)polymorphic derived type variable
!*                                    with unformatted I/O
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
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar001akl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b2 ! tcx: (3)

   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)
   type(child(3,4))              :: c2 = child(3,4) ( 'jkl', 1003 ) ! tcx: (3,4) ! tcx: (3,4)

   integer :: i1, i2

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   b2 = base(3)('def') ! tcx: (3)
   allocate ( c1, source = child(3,4) ('ghi',1001 ) ) ! tcx: (3,4)

   open ( 1, file = 'scalar001akl.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )                    b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 101_4

   write ( 1, iostat = stat, iomsg = msg )                    1000, c1, 1002, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )          error stop 2_4

   deallocate ( b1 )
   allocate ( b1, source = child(3,4) ( 'mno',1004 ) ) ! tcx: (3,4)

   write ( 1, iostat = stat, iomsg = msg )                    b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 3_4

   rewind 1

   deallocate ( b1 )
   allocate ( base(3) :: b1 ) ! tcx: (3)
   b2 = base(3)() ! tcx: (3)

   read ( 1, iostat = stat, iomsg = msg )                     b1, b2

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 4_4
   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) )            error stop 5_4

   read ( 1, iostat = stat, iomsg = msg )                     i1, c1, i2, c2

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )           error stop 6_4
   if ( ( i1 /= 1000 ) .or. ( i2 /= 1002 ) .or. &
        ( c1%c /= 'ghi' ) .or. ( c1%i /= 1001 ) .or. &
        ( c2%c /= 'jkl' ) .or. ( c2%i /= 1003 )      )        error stop 7_4

   deallocate ( b1 )
   allocate ( child(3,4) :: b1 ) ! tcx: (3,4)
   b2 = base(3)() ! tcx: (3)

   read ( 1, iostat = stat, iomsg = msg )                     b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 8_4

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'mno' ) .or. ( b1%i /= 1004 ) .or. ( b2%c /= 'def' ) )            error stop 9_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 9 changes
