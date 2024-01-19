!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures (generic-binding)
!*                               Stmt by Stmt: (Pg.60 14-15) An extended type includes all of the type
!*                                                           parameters, all of the components, and the
!*                                                           nonoverridden (4.5.6.2) nonfinal procedure
!*                                                           bindings of its parent type.
!*
!*                                             - Extension type define a specific binding overridding parent's specific binding
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

   type :: base
      character(3) :: c = 'xxx'
      contains
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child
      integer :: i = -999
      contains
         procedure, pass :: read => readchild    !<- override parent's read binding
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtiowritebase'
            type is ( child )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtiowritechild'
         end select

      end subroutine


      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadbase'

      end subroutine

      subroutine readchild (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadchild'

      end subroutine

end module

program extend002
   use m

   class(base), allocatable :: b1
   class(child), pointer    :: c1

   integer :: stat
   character(200) :: msg

   open ( 101, file = 'extend002.1', form='formatted', access='sequential' )

   allocate ( b1, source = base ('abc') )
   allocate ( c1, source = child('ABC', 201 ) )

   write ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritebase' ) ) error stop 1_4

   write ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 2_4

   deallocate ( b1 )
   allocate ( b1, source = child ( 'DEF', 202 ) )

   write ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritechild' ) )error stop 3_4

   rewind 101

   deallocate ( b1, c1 )
   allocate ( b1, c1 )

   read ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadbase' ) ) error stop 4_4

   if ( b1%c /= 'abc' ) error stop 5_4

   read ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 6_4

   if ( ( c1%c /= 'ABC' ) .or. ( c1%i /= 201 ) )       error stop 7_4

   deallocate ( b1 )
   allocate ( b1, source = child () )

   read ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 8_4

   select type ( b1 )
      type is ( child )
         if ( ( b1%c /= 'DEF' ) .or. ( b1%i /= 202 ) )       error stop 9_4
   end select

   close ( 101, status = 'delete' )

end program
