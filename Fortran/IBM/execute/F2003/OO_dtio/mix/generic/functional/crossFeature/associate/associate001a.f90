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
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Associate Construct
!*                                    -  selector is a scalar entity with unformatted i/o

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

program associate001a
   use m

   class(base), allocatable :: b1
   type(base)               :: b2

   class(child), pointer    :: c1
   type(child)              :: c2 = child ( 'jkl', 1003 )

   integer :: stat
   character(200) :: msg

   allocate ( b1, source = base ('abc') )
   b2 = base('def')
   allocate ( c1, source = child ('ghi',1001 ) )

   open ( 1, file = 'associate001a.1', form='unformatted', access='stream' )

   associate ( a => b1, b => b2, c => c1, d => c2 )

      write ( 1, iostat = stat, iomsg = msg, pos = 1  )      a, b
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )       error stop 1_4

      associate ( e => c, f => d )
         write ( 1, iostat = stat, iomsg = msg, pos = 20 )   e, f
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 2_4
      end associate

   end associate

   deallocate ( b1 )
   allocate ( b1, source = child ( 'mno',1004 ) )

   associate ( g => b1, h => b2, i => base('pqr'), j => child('stu',1005) )
      write ( 1, iostat = stat, iomsg = msg, pos = 40 )      g, h, i, j
      if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )       error stop 3_4
   end associate

   rewind 1

   deallocate ( b1, c1 )
   allocate ( base :: b1 )
   allocate ( child :: c1 )
   b2 = base()
   c2 = child()

   associate ( aa => b1, bb => b2, cc => c1, dd => c2 )

      read ( 1, iostat = stat, iomsg = msg, pos = 1  )      aa, bb
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )       error stop 4_4

      associate ( ee => cc, ff => dd )
         read ( 1, iostat = stat, iomsg = msg, pos = 20  )   ee, ff
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )    error stop 5_4
      end associate

   end associate

   if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. &
        ( c1%c /= 'ghi' ) .or. ( c1%i /= 1001 )  .or. &
        ( c2%c /= 'jkl' ) .or. ( c2%i /= 1003 ) )            error stop 6_4

   deallocate ( b1 )
   allocate ( child :: b1 )
   b2 = base()

   associate ( g => b1, h => b2 )
      read ( 1, iostat = stat, iomsg = msg, pos = 40 )      g,h
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )       error stop 7_4
   end associate

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'mno' ) .or. ( b1%i /= 1004 ) .or. ( b2%c /= 'def' ) )            error stop 8_4
   end select
   
   associate ( g => b1, h => b2 )
      read ( 1, iostat = stat, iomsg = msg, pos = 50 )      h, g
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )       error stop 9_4
   end associate

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'stu' ) .or. ( b1%i /= 1005 ) .or. ( b2%c /= 'pqr' ) )            error stop 10_4
   end select

   close ( 1, status ='delete')

end program
