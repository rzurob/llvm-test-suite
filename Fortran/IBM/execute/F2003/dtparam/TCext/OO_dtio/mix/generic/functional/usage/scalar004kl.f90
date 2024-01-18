!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar004kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar004 by Robert Ma)
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
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar unlimited polymorphic variable
!*                                    with formatted I/O
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
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
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

program scalar004kl
   use m

   class(*), allocatable :: u1
   class(*), pointer     :: u2

   integer :: i1, i2

   integer :: stat
   character(200) :: msg

   allocate ( u1, source = base(3) ('abc') ) ! tcx: (3)
   allocate ( u2, source = child(3,4) ('def',1001 ) ) ! tcx: (3,4)

   open ( 1, file = 'scalar004kl.1', form='formatted', access='sequential' )

   select type ( u1 )
      class is ( base(*) ) ! tcx: (*)
         select type ( g => u2 )
            type is ( child(*,4) ) ! tcx: (*,4)
               write ( 1, "(I4,1X,DT,I4,1X,DT)", iostat = stat, iomsg = msg )   1000, u1, 1002, g
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )                error stop 101_4
         end select
   end select

   select type ( u1 )
      class is ( base(*) ) ! tcx: (*)
         select type ( u2 )
            class is ( base(*) ) ! tcx: (*)
               write ( 1, *, iostat = stat, iomsg = msg )                u1, u2
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )         error stop 2_4
         end select
   end select

   rewind 1

   deallocate ( u1, u2 )
   allocate ( base(3)  :: u1 ) ! tcx: (3)
   allocate ( child(3,4) :: u2 ) ! tcx: (3,4)

   select type ( u1 )
      type is ( base(*) ) ! tcx: (*)
         select type ( g => u2 )
            type is ( child(*,4) ) ! tcx: (*,4)
               read ( 1, "(I4,1X,DT,I4,1X,DT)", iostat = stat, iomsg = msg )    i1, u1, i2, g
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                 error stop 3_4
               if ( ( i1 /= 1000 ) .or. ( u1%c /= 'abc' ) .or. ( i2 /= 1002 ) .or. ( g%c /= 'def' ) .or. ( g%i /= 1001 ) ) error stop 4_4
         end select
   end select

      select type ( u1 )
      type is ( base(*) ) ! tcx: (*)
         select type ( g => u2 )
            type is ( child(*,4) ) ! tcx: (*,4)
               read ( 1, "(2(1X,DT))", iostat = stat, iomsg = msg )                u1, g
               if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                    error stop 5_4
               if ( ( u1%c /= 'abc' ) .or. ( g%c /= 'def' ) .or. ( g%i /= 1001 ) ) error stop 6_4
         end select
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 7 changes
