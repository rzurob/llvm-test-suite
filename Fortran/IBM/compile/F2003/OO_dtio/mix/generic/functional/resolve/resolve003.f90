!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 9.5.3.7.3 Resolving derived-type input/output procedure references (generic binding)
!*                                    - Make both generic type bound and interface available
!*                                        - for formatted I/O with multiple extended types with arrays
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
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      character(3) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type, extends(child) :: gen3
      character(3) :: e = 'xxx'
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read => readg
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%d
         iomsg = 'dtioreadc'

      end subroutine

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,A3,1X,A3)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%d, dtv%e
         iomsg = 'dtioreadg'

      end subroutine

end module

program resolve003
   use m

   interface write(formatted)
      subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), allocatable  :: b1(:)
   class(child), pointer     :: c1(:,:)
   class(gen3), allocatable  :: g1(:)

   namelist /n1/ b1, c1, g1

   integer :: stat
   character(200) :: msg

   allocate ( b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( c1(2,2), source = reshape ( source = (/ child('abc','ABC'),child('def','DEF'), child('ghi','GHI'), child('jkl','JKL') /), shape= (/2,2/) ) )
   allocate ( g1(2), source = (/ gen3('abc','def','ghi'), gen3('jkl','mno','pqr') /) )

   open ( 1, file = 'resolve003.1', form='formatted', access='sequential' )

   write ( 1, *, iostat = stat, iomsg = msg )            b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )     error stop 1_4

   write ( 1, * , iostat = stat, iomsg = msg )           c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )     error stop 2_4

   write ( 1, "(1X,DT)" , iostat = stat, iomsg = msg )   g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )     error stop 3_4

   deallocate ( b1, c1, g1 )
   allocate ( b1(3), c1(2,2), g1(2) )

   rewind 1

   read ( 1, "(9(1X,DT))", iostat = stat, iomsg = msg )            b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )      error stop 4_4
   if ( ( b1(1)%c /='abc' ) .or. &
        ( b1(2)%c /='def' ) .or. &
        ( b1(3)%c /='ghi' ) .or. &
        ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%d /= 'ABC' ) .or. &
        ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%d /= 'DEF' ) .or. &
        ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%d /= 'GHI' ) .or. &
        ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%d /= 'JKL' ) .or. &
        ( g1(1)%c /='abc' ) .or. ( g1(1)%d /= 'def' ) .or. ( g1(1)%e /= 'ghi' )  .or. &
        ( g1(2)%c /='jkl' ) .or. ( g1(2)%d /= 'mno' ) .or. ( g1(2)%e /= 'pqr' ) ) error stop 5_4

   deallocate ( b1 )
   allocate ( child :: b1(4) )

   read ( 1, "(4(1X,DT))", iostat = stat, iomsg = msg )             b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )      error stop 6_4

   select type ( b1 )
      type is ( child )
         if ( ( b1(1)%c /= 'abc' ) .or. ( b1(1)%d /= 'ABC' ) .or. &
              ( b1(2)%c /= 'def' ) .or. ( b1(2)%d /= 'DEF' ) .or. &
              ( b1(3)%c /= 'ghi' ) .or. ( b1(3)%d /= 'GHI' ) .or. &
              ( b1(4)%c /= 'jkl' ) .or. ( b1(4)%d /= 'JKL' )  )  error stop 7_4
      class default
         error stop 8_4
   end select

   deallocate ( b1 )
   allocate ( gen3 :: b1(2) )

   read ( 1, "(1X,DT)", iostat = stat, iomsg = msg )     b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )      error stop 8_4

   select type ( b1 )
      type is ( gen3 )
         if ( ( b1(1)%c /='abc' ) .or. ( b1(1)%d /= 'def' ) .or. ( b1(1)%e /= 'ghi' )  .or. &
              ( b1(2)%c /='jkl' ) .or. ( b1(2)%d /= 'mno' ) .or. ( b1(2)%e /= 'pqr' ) )  error stop 9_4
      class default
         error stop 10_4
   end select

   close ( 1, status ='delete')

end program

subroutine writebaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 11_4
   iomsg = 'ERROR'

end subroutine

subroutine readbaseext (dtv, unit, iotype, v_list, iostat, iomsg)
   use m
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   error stop 12_4
   iomsg = 'ERROR'

end subroutine
