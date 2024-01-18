!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - array (non-)polymorphic derived type variable
!*                                    with unformatted I/O
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
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

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

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program array001a
   use m

   class(base), allocatable :: b1(:)
   type(base)               :: b2(3)

   class(child), pointer    :: c1(:,:)
   type(child)              :: c2(3) = (/ child ( 'ABC', 201 ), child ( 'DEF', 202 ), child ( 'GHI', 203 )  /)

   integer :: stat
   character(200) :: msg

   allocate ( b1(3), source = (/ base ('abc'), base ('def'), base('ghi') /) )
   b2 = (/ base ('ABC'), base ('DEF'), base('GHI') /)

   allocate ( c1(2,2), source = reshape ( source = (/ child ('abc',101 ), child ('def',102 ), child ('ghi',103 ), child ('jkl',104 ) /) , shape = (/2,2/) ) )

   open ( 1, file = 'array001a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )                 b1(1:3), b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )       error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )                 c1, c2((/1,2,3/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )       error stop 2_4

   deallocate ( b1 )
   allocate ( b1(2), source = (/ child ( 'mno',105 ), child ( 'pqr',106 ) /) )

   write ( 1, iostat = stat, iomsg = msg )                 b1((/1,2/))
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )       error stop 3_4

   rewind 1

   deallocate ( b1, c1 )
   allocate ( base :: b1(3) )
   allocate ( child :: c1(2,2) )
   b2 = base()
   c2 = child()

   read ( 1, iostat = stat, iomsg = msg )                  b1, b2(1:3)

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )        error stop 4_4
   if ( ( b1(1)%c /= 'abc' ) .or. &
        ( b1(2)%c /= 'def' ) .or. &
        ( b1(3)%c /= 'ghi' ) .or. &
        ( b2(1)%c /= 'ABC' ) .or. &
        ( b2(2)%c /= 'DEF' ) .or. &
        ( b2(3)%c /= 'GHI' ) )                             error stop 5_4

   read ( 1, iostat = stat, iomsg = msg )                  c1, c2(1:3)

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )        error stop 6_4
   if ( ( c1(1,1)%c /= 'abc' ) .or. ( c1(1,1)%i /= 101 ) .or. &
        ( c1(2,1)%c /= 'def' ) .or. ( c1(2,1)%i /= 102 ) .or. &
        ( c1(1,2)%c /= 'ghi' ) .or. ( c1(1,2)%i /= 103 ) .or. &
        ( c1(2,2)%c /= 'jkl' ) .or. ( c1(2,2)%i /= 104 ) .or. &
        ( c2(1)%c /= 'ABC' )   .or. ( c2(1)%i /= 201 )   .or. &
        ( c2(2)%c /= 'DEF' )   .or. ( c2(2)%i /= 202 )   .or. &
        ( c2(3)%c /= 'GHI' )   .or. ( c2(3)%i /= 203 ) )   error stop 7_4

   deallocate ( b1 )
   allocate ( child :: b1(2) )

   read ( 1, iostat = stat, iomsg = msg )                  b1(2:1:-1)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )        error stop 8_4

   select type ( b1 )
      type is ( child )
         if ( ( b1(2)%c /= 'mno' ) .or. ( b1(2)%i /= 105 )   .or. &
              ( b1(1)%c /= 'pqr' ) .or. ( b1(1)%i /= 106 ) )  error stop 9_4
   end select

   close ( 1, status ='delete')

end program
