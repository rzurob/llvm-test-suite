!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array derived type variable containing polymorphic
!*                                    components which has DTIO with unformatted I/O
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
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   type, extends(base) :: child
      character(3) :: d = 'xxx'
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type :: container
      integer :: i
      class(base), allocatable :: b
      contains
         procedure, pass :: write => writecon
         procedure, pass :: read => readcon
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   contains

      subroutine writecon (dtv, unit, iostat, iomsg)
         class(container), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%i, dtv%b

         select type ( g => dtv%b )
            type is ( base )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowriteb' ) ) error stop 1_4
            type is ( child )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtiowritec' ) ) error stop 2_4
         end select

         iomsg = 'dtiowritecon'

      end subroutine

      subroutine readcon (dtv, unit, iostat, iomsg)
         class(container), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%i, dtv%b

         select type ( g => dtv%b )
            type is ( base )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadb' ) ) error stop 3_4
            type is ( child )
               if ( ( iostat /= 0 ) .or. ( iomsg /= 'dtioreadc' ) ) error stop 4_4
         end select

         iomsg = 'dtioreadcon'


      end subroutine

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

end module

program array003a
   use m

   class(container), allocatable :: b1(:)
   type(container) :: b2(2,2)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'array003a.1', form='unformatted', access='sequential' )

   allocate ( b1(3), source = (/ container( 101, base('abc') ), container( 102, base('def') ), container( 103, base('ghi') ) /) )
   b2 = reshape( source = (/ container(201, child('ABC','abc')), container(202, child('DEF','def')), &
                             container(203, child('GHI','ghi')), container(204, child('JKL','jkl')) /), shape = (/ 2,2 /) )
   write ( 1, iostat = stat, iomsg = msg )                b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritecon' ) )    error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )                b2            !<- reversion occurs
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritecon' ) )    error stop 2_4

   rewind 1

   deallocate ( b1 )
   allocate ( b1(3) )
   allocate ( base :: b1(1)%b, b1(2)%b, b1(3)%b )

   b2 = container(-999,base())

   read ( 1, iostat = stat, iomsg = msg )                   b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadcon' ) )         error stop 3_4

   read ( 1, iostat = stat, iomsg = msg )   b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadcon' ) )         error stop 4_4

   if ( ( b1(1)%i /= 101 ) .or. ( b1(1)%b%c /= 'abc' ) .or. &
        ( b1(2)%i /= 102 ) .or. ( b1(2)%b%c /= 'def' ) .or. &
        ( b1(3)%i /= 103 ) .or. ( b1(3)%b%c /= 'ghi' ) )    error stop 5_4

   select type ( g => b2(1,1)%b )
      type is ( child )
         if ( ( b2(1,1)%i /= 201 ) .or. ( g%c /= 'ABC' ) .or. ( g%d /= 'abc' ) )   error stop 6_4
   end select

   select type ( g => b2(2,1)%b )
      type is ( child )
         if ( ( b2(2,1)%i /= 202 ) .or. ( g%c /= 'DEF' ) .or. ( g%d /= 'def' ) )   error stop 7_4
   end select

   select type ( g => b2(1,2)%b )
      type is ( child )
         if ( ( b2(1,2)%i /= 203 ) .or. ( g%c /= 'GHI' ) .or. ( g%d /= 'ghi' ) )   error stop 8_4
   end select

   select type ( g => b2(2,2)%b )
      type is ( child )
         if ( ( b2(2,2)%i /= 204 ) .or. ( g%c /= 'JKL' ) .or. ( g%d /= 'jkl' ) )   error stop 9_4
   end select

   close ( 1, status ='delete')

end program