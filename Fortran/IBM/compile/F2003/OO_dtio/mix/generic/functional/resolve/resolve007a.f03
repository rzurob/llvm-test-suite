!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Generic type bound is available in extended types, and interface available for base type
!*                                        - for unformatted I/O, Outside select type construct -> interface used
!*                                                             Inside select type construct -> generic used
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
   end type

   type, extends(base) :: child
      character(3) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(child) :: gen3
      character(3) :: e = 'xxx'
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read => readg
   end type

   contains

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d
         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d, dtv%e
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iostat, iomsg)
         class(gen3), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg)    dtv%c, dtv%d, dtv%e
         iomsg = 'dtioreadg'

      end subroutine

end module

program resolve007a
   use m

   interface write(unformatted)
      subroutine writebaseext (dtv, unit, iostat, iomsg)
         use m, only: base, child
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface write(unformatted)

   interface read(unformatted)
      subroutine readbaseext (dtv, unit, iostat, iomsg)
         use m, only: base, child
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface read(unformatted)

   class(base), allocatable  :: b1
   class(base), pointer      :: b2

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base('abc') )

   open ( 1, file = 'resolve007a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )      b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowriteb' ) ) error stop 1_4

   deallocate ( b1 )
   allocate ( b1, source = child('def','ghi') )

   write ( 1, iostat = stat, iomsg = msg )      b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowritec' ) ) error stop 2_4

   select type ( b1 )
      class is ( child )
         write ( 1, iostat = stat, iomsg = msg )      b1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 3_4
   end select

   deallocate ( b1 )
   allocate ( b1, source = gen3('jkl','mno','pqr') )

   write ( 1, iostat = stat, iomsg = msg )      b1
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtiowriteg' ) ) error stop 4_4

   select type ( b1 )
      type is ( gen3 )
         write ( 1, iostat = stat, iomsg = msg )      b1
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 5_4
   end select

   rewind 1

   allocate ( b2 )

   read ( 1, iostat = stat, iomsg = msg )      b2
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioreadb' ) ) error stop 6_4

   if ( b2%c /= 'abc' ) error stop 7_4

   deallocate ( b2 )
   allocate ( child :: b2 )

   read ( 1, iostat = stat, iomsg = msg )      b2
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioreadc' ) ) error stop 7_4

   select type ( b2 )
      class is ( child )

         if ( ( b2%c /= 'def' ) .or. ( b2%d /= 'ghi' ) ) error stop 8_4
         b2%c = 'xxx'
         b2%d = 'xxx'

         read( 1, iostat = stat, iomsg = msg )      b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )    error stop 9_4

         if ( ( b2%c /= 'def' ) .or. ( b2%d /= 'ghi' ) ) error stop 10_4

   end select

   deallocate ( b2 )
   allocate ( b2, source = gen3('jkl','mno','pqr') )

   read ( 1, iostat = stat, iomsg = msg )      b2
   if ( ( stat /= 0 ) .or. ( msg /= 'extdtioreadg' ) ) error stop 11_4

   select type ( b2 )
      type is ( gen3 )

         if ( ( b2%c /= 'jkl' ) .or. ( b2%d /= 'mno' ) .or. ( b2%e /= 'pqr' ) ) error stop 12_4
         b2%c = 'xxx'
         b2%d = 'xxx'
         b2%e = 'xxx'

         read ( 1, iostat = stat, iomsg = msg )      b2
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )    error stop 13_4
         if ( ( b2%c /= 'jkl' ) .or. ( b2%d /= 'mno' ) .or. ( b2%e /= 'pqr' ) ) error stop 14_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iostat, iomsg)
   use m, only: base, child, gen3
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   selecttype ( dtv )
      type is (base)
         write (unit, iostat=iostat, iomsg=iomsg)             dtv%c
         iomsg = 'extdtiowriteb'
      type is (child)
         write (unit, iostat=iostat, iomsg=iomsg)       dtv%c, dtv%d
         iomsg = 'extdtiowritec'
      type is (gen3)
         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'extdtiowriteg'
   end select

end subroutine

subroutine readbaseext (dtv, unit, iostat, iomsg)
   use m, only: base, child, gen3
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         read (unit, iostat=iostat, iomsg=iomsg)             dtv%c
         iomsg = 'extdtioreadb'
      type is (child)
         read (unit, iostat=iostat, iomsg=iomsg)       dtv%c, dtv%d
         iomsg = 'extdtioreadc'
      type is (gen3)
         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'extdtioreadg'
   end select

end subroutine
