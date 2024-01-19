!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - specific binding referring to a external procedure
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
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
   end type

   interface
      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific005
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable :: b1
   class(child), pointer    :: c1

   allocate ( b1, source = base('abc') )
   allocate ( c1, source = child('def', 1001) )

   open ( 1, file = 'specific005.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) )    error stop 2_4

   rewind 1

   read (1, *, iostat=stat, iomsg = msg)  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 3_4

   read (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) )     error stop 4_4

   if ( ( b1%c /= 'def' ) .or. ( c1%c /= 'abc' ) .or. ( c1%i /= 9999 ) ) error stop 5_4

   close ( 1, status ='delete')

end program

subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base )
         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, 9999
      type is ( child )
         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   end select

   iomsg = 'dtiowrite'

end subroutine

subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is ( base )
         read (unit, "(A3,5X)" , iostat=iostat, iomsg=iomsg) dtv%c
      type is ( child )
         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   end select

   iomsg = 'dtioread'

end subroutine
