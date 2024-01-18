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
!*                                  Usage:
!*                                    -  Scalar entity containing sequence scalar/array components with formatted i/o
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

   type data
      sequence
      integer(8) :: i = -999
   end type

   type base
      type(data) :: s1 = data()
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child
      type(data), allocatable :: s2(:)
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

         character(20) :: fmt

         write (fmt, *) "(I", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%s1

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(I", v_list(1), ",/, (I ",v_list(2)," ))"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%s1, dtv%s2

         iomsg = 'dtiowritec'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(I", v_list(1), ")"
         read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%s1

         iomsg = 'dtioreadb'

      end subroutine

      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(I", v_list(1), ",/, (I ",v_list(2)," ))"
         read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%s1, dtv%s2

         iomsg = 'dtioreadc'

      end subroutine

end module

program scalar007
   use m

   integer :: stat
   character(200) :: msg

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   open ( 1, file = 'scalar007.1', form='formatted', access='sequential' )

   allocate ( b1, source = base (data(101)) )
   allocate ( b2, source = child(data(201), (/ data(20001), data(20002), data(20003) /) ) )

   write ( 1, "(DT(3))", iostat = stat, iomsg = msg )           b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )            error stop 1_4

   write ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )         b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )            error stop 2_4

   rewind 1

   deallocate ( b1, b2 )
   allocate ( b1, source = base() )
   allocate ( b2, source = child(data(),(/(data(),i=1,3)/)) )

   read ( 1, "(DT(3))", iostat = stat, iomsg = msg )           b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )           error stop 3_4

   read ( 1, "(DT(3,5))", iostat = stat, iomsg = msg )         b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )           error stop 4_4

   if ( b1%s1%i /= 101 ) error stop 5_4

   select type ( b2 )
      type is ( child )
         if ( ( b2%s1%i /= 201 ) .or. ( b2%s2(1)%i /= 20001 ) .or. ( b2%s2(2)%i /= 20002 ) .or. ( b2%s2(3)%i /= 20003 ) ) error stop 6_4
   end select

   close ( 1, status ='delete')

end program
