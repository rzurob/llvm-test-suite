!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate001kl
!*
!*  PROGRAMMER                 : David Forster (derived from associate001 by Robert Ma)
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
!*                                    -  selector is a scalar entity with formatted i/o

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

program associate001kl
   use m

   class(base(:)), allocatable :: b1 ! tcx: (:)
   type(base(3))               :: b2 ! tcx: (3)

   class(child(:,4)), pointer    :: c1 ! tcx: (:,4)
   type(child(3,4))              :: c2 = child(3,4) ( 'jkl', 1003 ) ! tcx: (3,4) ! tcx: (3,4)

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   b2 = base(3)('def') ! tcx: (3)
   allocate ( c1, source = child(3,4) ('ghi',1001 ) ) ! tcx: (3,4)

   open ( 1, file = 'associate001kl.1', form='formatted', access='stream' )

   associate ( a => b1, b => b2, c => c1, d => c2 )

      write ( 1, "(DT,DT)", iostat = stat, iomsg = msg, pos = 1  )      a, b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )       error stop 101_4

      associate ( e => c, f => d )
         write ( 1, "(DT,DT)", iostat = stat, iomsg = msg, pos = 20 )   e, f
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 2_4
      end associate

   end associate

   deallocate ( b1 )
   allocate ( b1, source = child(3,4) ( 'mno',1004 ) ) ! tcx: (3,4)

   associate ( g => b1, h => b2, i => base(3)('pqr'), j => child(3,4)('stu',1005) ) ! tcx: (3) ! tcx: (3,4)
      write ( 1, "(4(DT))", iostat = stat, iomsg = msg, pos = 40 )      g, h, i, j
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )       error stop 3_4
   end associate

   rewind 1

   deallocate ( b1, c1 )
   allocate ( base(3) :: b1 ) ! tcx: (3)
   allocate ( child(3,4) :: c1 ) ! tcx: (3,4)
   b2 = base(3)() ! tcx: (3)
   c2 = child(3,4)() ! tcx: (3,4)

   associate ( aa => b1, bb => b2, cc => c1, dd => c2 )

      read ( 1, "(DT,DT)", iostat = stat, iomsg = msg, pos = 1  )      aa, bb
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )       error stop 4_4

      associate ( ee => cc, ff => dd )
         read ( 1, "(DT,DT)", iostat = stat, iomsg = msg, pos = 20  )   ee, ff
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )    error stop 5_4
      end associate

   end associate

   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. &
        ( c1%c /= 'ghi' ) .or. ( c1%i /= 1001 )  .or. &
        ( c2%c /= 'jkl' ) .or. ( c2%i /= 1003 ) )            error stop 6_4

   deallocate ( b1 )
   allocate ( child(3,4) :: b1 ) ! tcx: (3,4)
   b2 = base(3)() ! tcx: (3)

   associate ( g => b1, h => b2 )
      read ( 1, "(2(DT))", iostat = stat, iomsg = msg, pos = 40 )      g,h
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )       error stop 7_4
   end associate

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'mno' ) .or. ( b1%i /= 1004 ) .or. ( b2%c /= 'def' ) )            error stop 8_4
   end select
   
   associate ( g => b1, h => b2 )
      read ( 1, "(2(DT))", iostat = stat, iomsg = msg, pos = 51 )      h, g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )       error stop 9_4
   end associate

   select type ( b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1%c /= 'stu' ) .or. ( b1%i /= 1005 ) .or. ( b2%c /= 'pqr' ) )            error stop 10_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 10 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 13 changes
