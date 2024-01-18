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
!*  DESCRIPTION                : Usage of GENERIC BINDING
!*                                  - scalar variables with io-implied-do
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

   type newbase
      real(4) :: r =  -999.0
      contains
      procedure, pass :: write => writen
      procedure, pass :: read => readn
      generic :: write(unformatted) => write
      generic :: read(unformatted)  => read
   end type

   contains

      subroutine writen (dtv, unit, iostat, iomsg)
         class(newbase), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%r

         iomsg = 'dtiowriten'

      end subroutine

      subroutine readn (dtv, unit, iostat, iomsg)
         class(newbase), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%r
         iomsg = 'dtioreadn'

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

program scalar005a
   use m

   integer :: stat
   character(200) :: msg

   class(base), allocatable :: b1, b2
   class(newbase), pointer  :: n1, n2

   logical :: precision_r4

   open ( 1, file = 'scalar005a.1', form='unformatted', access='sequential' )

   allocate ( b1, source = base ('abc') )
   allocate ( b2, source = child('def',101) )
   allocate ( n1, source = newbase (1.0) )
   allocate ( n2, source = newbase (2.0) )

   write ( 1, iostat = stat, iomsg = msg )   ( b1, n1, i = -5, -4 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriten' ) )        error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )   ( n2, b2, i =100,99,-1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )        error stop 2_4

   rewind 1

   deallocate ( b1, b2, n1, n2 )
   allocate ( b1, n1, n2 )
   allocate ( child :: b2 )

   read ( 1, iostat = stat, iomsg = msg )         ( b1, n1, i = -5, -4 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadn' ) )        error stop 3_4

   read ( 1, iostat = stat, iomsg = msg )                 ( n2, b2, i =100,99,-1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )        error stop 4_4

   select type ( b2 )
      type is ( child )
         if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 101 ) .or. &
              ( .not. precision_r4(n1%r, 1.0) ) .or. ( .not. precision_r4(n2%r, 2.0) ) ) error stop 5_4
   end select

   close ( 1, status ='delete')

end program
