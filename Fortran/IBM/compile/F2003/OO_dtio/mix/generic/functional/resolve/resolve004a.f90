!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Generic type bound is private and interface is public
!*                                        - for unformatted I/O, inside module, generic tb should be called
!*                                          outside module, interface dtio shall be called
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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic, private :: write(unformatted) => write
         generic, private :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      character(3) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   interface write(unformatted)
      subroutine writebaseext (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(unformatted)
      subroutine readbaseext (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtioreadc'

      end subroutine

      subroutine myread( dtv, iostat, iomsg )
         class(base), intent(inout) :: dtv
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read ( 1, iostat = iostat, iomsg = iomsg ) dtv

      end subroutine

      subroutine mywrite( dtv, iostat, iomsg )
         class(base), intent(in) :: dtv
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( 1, iostat = iostat, iomsg = iomsg ) dtv

      end subroutine

end module

program resolve004
   use m

   class(base), allocatable  :: b1
   class(child), pointer     :: c1

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base('abc') )
   allocate ( c1, source = child('def','ghi') )

   open ( 1, file = 'resolve004.1', form='unformatted', access='sequential' )

   call mywrite ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

   call mywrite ( c1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 2_4

   deallocate ( b1 )
   allocate ( b1, source = child('jkl','mno') )

   call mywrite ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) ) error stop 3_4

   deallocate ( b1 )
   allocate ( b1, source = base('abc') )

   write ( 1, iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 4_4

   write ( 1, iostat = stat, iomsg = msg )   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 5_4

   deallocate ( b1 )
   allocate ( b1, source = child('jkl','mno') )

   write ( 1, iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 6_4

   rewind 1

   deallocate ( b1, c1 )
   allocate ( b1, c1 )

   call myread ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 7_4
   if ( b1%c /= 'abc' )                             error stop 8_4

   call myread ( c1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 9_4
   if ( ( c1%c /= 'def' ) .or. ( c1%d /= 'ghi' ) )  error stop 10_4

   deallocate ( b1 )
   allocate ( child :: b1 )

   call myread ( b1, stat, msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) ) error stop 11_4
   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'jkl' ) .or. ( b1%d /= 'mno' ) )  error stop 12_4
      class default
         error stop 13_4
   end select

   deallocate ( b1, c1 )
   allocate ( b1, c1 )

   read ( 1, iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioread' ) ) error stop 14_4
   if ( b1%c /= 'abc' )                               error stop 15_4

   read ( 1, iostat = stat, iomsg = msg )   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioread' ) ) error stop 16_4
   if ( ( c1%c /= 'def' ) .or. ( c1%d /= 'ghi' ) )    error stop 17_4

   deallocate ( b1 )
   allocate ( b1, source = child('jkl','mno') )

   write ( 1, iostat = stat, iomsg = msg )   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowrite' ) ) error stop 18_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'jkl' ) .or. ( b1%d /= 'mno' ) )  error stop 19_4
      class default
         error stop 20_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iostat, iomsg)
   use m, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         write (unit, iostat=iostat, iomsg=iomsg)       dtv%c
      type is (child)
         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
   end select

   iomsg = 'extdtiowrite'

end subroutine

subroutine readbaseext (dtv, unit, iostat, iomsg)
   use m, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         read (unit, iostat=iostat, iomsg=iomsg)       dtv%c
      type is (child)
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
   end select

   iomsg = 'extdtioread'

end subroutine
