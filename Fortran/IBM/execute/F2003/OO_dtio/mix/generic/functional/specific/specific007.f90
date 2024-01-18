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
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                                 - Specific Binding
!*                                    - specific binding referring to a external procedure child type and module procedure for parent type
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

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, 9999
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,5X)" , iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

end module

module m1
   use m, only: base

   type, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
   end type

   interface
      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface
      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific007
   use m1

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable :: b1, b2
   class(child), pointer    :: c1

   allocate ( b1, source = base('abc') )
   allocate ( b2, source = child('def', 1001) )
   allocate ( c1, source = child('ghi', 2001) )

   open ( 1, file = 'specific007.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1, b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 2_4

   rewind 1

   deallocate ( b1, b2, c1 )
   allocate ( c1, b2 )
   allocate ( child :: b1 )

   read (1, *, iostat=stat, iomsg = msg)   b2, b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )     error stop 3_4

   read (1, *, iostat=stat, iomsg = msg)   c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )     error stop 4_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'def' ) .or. ( b1%i /= 1001 ) .or. ( b2%c /= 'abc' ) .or. ( c1%c /= 'ghi' ) .or. ( c1%i /= 2001 ) ) error stop 5_4
   end select

   close ( 1, status ='delete')

end program


subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: child
   class(child), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtiowritec'

end subroutine

subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: child
   class(child), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtioreadc'

end subroutine
