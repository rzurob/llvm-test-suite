!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate002kl
!*
!*  PROGRAMMER                 : David Forster (derived from associate002 by Robert Ma)
!*  DATE                       : 2007-08-02 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Associate Construct
!*                                    -  selector is a array entity with formatted i/o

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

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kc) ! kc=4
      integer, kind :: kc
      integer(kc) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program associate002kl
   use m

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   type(base(3))               :: b2(3) ! tcx: (3)

   class(child(:,4)), pointer    :: c1(:,:) ! tcx: (:,4)
   type(child(3,4))              :: c2(3) = (/ child(3,4) ( 'abc', 2001 ), child(3,4) ( 'def', 2002 ), child(3,4) ( 'ghi', 2003 ) /) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   allocate ( b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   b2 = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( c1(2,2), source = reshape ( source = (/ child(3,4) ('ABC',1001 ), child(3,4) ('DEF',1002 ), & ! tcx: (3,4) ! tcx: (3,4)
                                                      child(3,4) ('GHI',1003 ), child(3,4) ('JKL',1004 ) /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'associate002kl.1', form='formatted', access='sequential' )

   associate ( a => b1((/1,2,3/)), b => b2, c => c1, d => c2(3:1:-1) )

      write ( 1, "(3(DT))", iostat = stat, iomsg = msg )      a, b  !<- reversion
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )       error stop 1_4

      associate ( e => c, f => d(3:1:-1) )
         write ( 1, "(4(DT),/,3(DT))", iostat = stat, iomsg = msg )   e, f
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 2_4
      end associate

   end associate

   deallocate ( b1 )
   allocate ( b1(3), source = (/ child(3,4) ( 'mno',1005 ), child(3,4) ( 'pqr', 1006), child(3,4) ( 'stu', 1007 ) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   call associate2 ( g = b1, h = (/ c1(1:2,1), c2(1) /) )

   rewind 1

   deallocate ( b1, c1 )
   allocate ( base(3) :: b1(3) ) ! tcx: (3)
   allocate ( child(3,4) :: c1(2,2) ) ! tcx: (3,4)
   b2 = base(3)() ! tcx: (3)
   c2 = child(3,4)() ! tcx: (3,4)

   associate ( aa => b1, bb => b2, cc => c1, dd => c2 )

      read ( 1, "(3(DT))", iostat = stat, iomsg = msg )      aa, bb
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )       error stop 4_4

      associate ( ee => cc, ff => dd )
         read ( 1, "(4(DT),/,3(DT))", iostat = stat, iomsg = msg )   ee, ff
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )    error stop 5_4
      end associate

   end associate

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) .or. &
        ( b2(1)%c /= 'ABC' ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(3)%c /= 'GHI' ) .or. &
        ( c1(1,1)%c /= 'ABC' ) .or. ( c1(2,1)%c /= 'DEF' ) .or. ( c1(1,2)%c /= 'GHI' ) .or. ( c1(2,2)%c /= 'JKL' ) .or. &
        ( c1(1,1)%i /= 1001 )  .or. ( c1(2,1)%i /= 1002 )  .or. ( c1(1,2)%i /= 1003 )  .or. ( c1(2,2)%i /= 1004 )  .or. &
        ( c2(1)%c /= 'abc' ) .or. ( c2(2)%c /= 'def' ) .or. ( c2(3)%c /= 'ghi' ) .or. &
        ( c2(1)%i /= 2001 )  .or. ( c2(2)%i /= 2002 )  .or. ( c2(3)%i /= 2003 ) )            error stop 6_4

   deallocate ( b1 )
   allocate ( child(3,4) :: b1(3) ) ! tcx: (3,4)
   c2 = child(3,4)() ! tcx: (3,4)

   call associate4 ( g = b1, h = c2 )

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1(1)%c /= 'mno' ) .or. ( b1(1)%i /= 1005 ) .or. &
              ( b1(2)%c /= 'pqr' ) .or. ( b1(2)%i /= 1006 ) .or. &
              ( b1(3)%c /= 'stu' ) .or. ( b1(3)%i /= 1007 ) .or. &
              ( c2(1)%c /= 'ABC' ) .or. ( c2(1)%i /= 1001 ) .or. &
              ( c2(2)%c /= 'DEF' ) .or. ( c2(2)%i /= 1002 ) .or. &
              ( c2(3)%c /= 'abc' ) .or. ( c2(3)%i /= 2001 ) )        error stop 8_4
   end select


   close ( 1, status ='delete')

   contains

!   associate ( g => b1, h => (/ c1(1:2,1), c2(1) /) )
   subroutine associate2 ( g, h )
    class (base(*)), intent(in) :: g(:)
    class(child(*,4)), intent(in) :: h(:)
      write ( 1, "(3(DT),/,3(DT))", iostat = stat, iomsg = msg )      g, h
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )       error stop 3_4
   end subroutine

!   associate ( g => b1, h => c2 )
   subroutine associate4 ( g, h )
    class(base(*)), intent(inout) :: g(:)
    type(child(*,4)), intent(inout) :: h(:)
      read ( 1, "(3(DT),/,3(DT))", iostat = stat, iomsg = msg )      g,h
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )         error stop 7_4
   end subroutine
end program
