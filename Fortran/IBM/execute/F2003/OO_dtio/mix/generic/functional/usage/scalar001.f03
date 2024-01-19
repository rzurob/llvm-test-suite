!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar (non-)polymorphic derived type variable
!*                                    with formatted I/O
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
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
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

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)" , iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar001
   use m

   class(base), allocatable :: b1
   type(base)               :: b2

   class(child), pointer    :: c1
   type(child)              :: c2 = child ( 'jkl', 1003 )

   integer :: i1, i2

   namelist /n1/ b1, b2

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ('abc') )
   b2 = base('def')
   allocate ( c1, source = child ('ghi',1001 ) )

   open ( 1, file = 'scalar001.1', form='formatted', access='sequential' )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 1_4

   write ( 1, "(I5,DT,I5,DT)", iostat = stat, iomsg = msg )   1000, c1, 1002, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )          error stop 2_4

   deallocate ( b1 )
   allocate ( b1, source = child ( 'mno',1004 ) )

   write ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )          error stop 3_4

   rewind 1

   deallocate ( b1 )
   allocate ( base :: b1 )
   b2 = base()

   read ( 1, n1, iostat = stat, iomsg = msg )

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 4_4
   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) )            error stop 5_4

   read ( 1, "(I5,DT,I5,DT)", iostat = stat, iomsg = msg )    i1, c1, i2, c2

   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )          error stop 6_4
   if ( ( i1 /= 1000 ) .or. ( i2 /= 1002 ) .or. &
        ( c1%c /= 'ghi' ) .or. ( c1%i /= 1001 ) .or. &
        ( c2%c /= 'jkl' ) .or. ( c2%i /= 1003 )      )        error stop 7_4

   deallocate ( b1 )
   allocate ( child :: b1 )
   b2 = base()

   read ( 1, n1, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 8_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'mno' ) .or. ( b1%i /= 1004 ) .or. ( b2%c /= 'def' ) )            error stop 9_4
   end select

   close ( 1, status ='delete')

end program
