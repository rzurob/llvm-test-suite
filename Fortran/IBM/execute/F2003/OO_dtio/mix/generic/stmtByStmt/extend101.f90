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
!*                                             - Extension type using parent's dtio generic binding (read)
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
         generic :: write(formatted) => write
         procedure, pass :: read => readbase
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child
      integer :: i = -99
   end type

   type, extends(child) :: gen3
      integer :: j = -99
   end type

   contains

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c
               iomsg = 'dtioreadbase'
            type is ( child )
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
               iomsg = 'dtioreadchild'
            type is ( gen3 )
               read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
               iomsg = 'dtioreadgen3'
         end select

      end subroutine

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
            type is ( child )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
            type is ( gen3 )
               write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         end select

      end subroutine
end module

program extend101
   use m

   class(base), allocatable :: b1
   class(child), pointer    :: c1
   class(gen3), allocatable :: g1

   type ( child ) :: c2 = child ()
   type ( gen3 )  :: g2 = gen3  ()

   namelist /b1nml/ b1

   integer :: stat
   character(200) :: msg

   open ( 101, file = 'extend101.1', form='formatted', access='sequential' )

   allocate ( b1, source = base () )
   allocate ( c1, source = child() )
   allocate ( g1, source = gen3 ( ) )

   read ( 101, *, iostat = stat, iomsg = msg )         b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadbase' ) ) error stop 1_4

   read ( 101, *, iostat = stat, iomsg = msg )         c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 2_4

   read ( 101, *, iostat = stat, iomsg = msg )         c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 3_4

   read ( 101, *, iostat = stat, iomsg = msg )         g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 4_4

   read ( 101, *, iostat = stat, iomsg = msg )         g2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 5_4

   print *, b1
   print *, c1
   print *, c2
   print *, g1
   print *, g2

   deallocate ( b1, c1 )

   allocate ( b1, source = child () )
   allocate ( c1, source = gen3 () )

   read ( 101, "(DT)", iostat = stat, iomsg = msg )    b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadchild' ) )error stop 6_4

   read ( 101, "(DT)", iostat = stat, iomsg = msg )    c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 7_4

   print *, b1
   print *, c1

   deallocate ( b1 )

   allocate ( b1, source = gen3 () )

   read ( 101, b1nml, iostat = stat, iomsg = msg )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadgen3' ) ) error stop 8_4

   print *, b1

end program
