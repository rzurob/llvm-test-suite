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
!*                                    - multiple type hierarchy, and all defined in different modules
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

         write (unit, "(A3)", iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3)" , iostat=iostat, iomsg=iomsg)    dtv%c
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

   contains

      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
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
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         iomsg = 'dtioreadc'

      end subroutine

end module

module m2
   use m1

   type, extends(child) :: gen3
      integer(4) :: j = -999
      contains
         procedure, pass :: write => writegen3
         procedure, pass :: read => readgen3
   end type

   contains

      subroutine writegen3 (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readgen3 (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i, dtv%j
         iomsg = 'dtioreadg'

      end subroutine

end module

program specific008
   use m2

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable :: b1, b2, b3
   class(child), pointer    :: c1, c2
   class(gen3), pointer     :: g1

   allocate ( b1, source = base ('abc') )
   allocate ( b2, source = child('def', 1001) )
   allocate ( b3, source = gen3 ('ghi', 1002, 1003) )
   allocate ( c1, source = child('ABC', 2001) )
   allocate ( c2, source = gen3 ('DEF', 2002, 2003) )
   allocate ( g1, source = gen3 ('GHI', 3001, 3002) )

   open ( 1, file = 'specific008.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1, b2, b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 2_4

   write (1, *, iostat=stat, iomsg = msg)  g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )    error stop 3_4

   rewind 1

   deallocate ( b1, b2, b3, c1, c2, g1 )

   allocate ( b1 )
   allocate ( child :: b2, c1 )
   allocate ( gen3 :: b3, c2, g1 )

   read (1, *, iostat=stat, iomsg = msg)   b1, b2, b3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )     error stop 4_4

   read (1, *, iostat=stat, iomsg = msg)   c1, c2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )     error stop 5_4

   read (1, *, iostat=stat, iomsg = msg)   g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) )     error stop 6_4

   select type ( b2 )
      type is ( child )
         select type ( b3 )
            type is ( gen3 )
               if ( ( b1%c /= 'abc' ) .or. ( b2%c /= 'def' ) .or. ( b2%i /= 1001 ) .or. ( b3%c /= 'ghi' ) .or. ( b3%i /= 1002 ) .or. ( b3%j /= 1003 ) ) error stop 7_4
         end select
   end select

   select type ( c2 )
      type is ( gen3 )
         if ( ( c1%c /= 'ABC' ) .or. ( c1%i /= 2001 ) .or. ( c2%c /= 'DEF' ) .or. ( c2%i /= 2002 ) .or. ( c2%j /= 2003 ) ) error stop 8_4
   end select

   if ( ( g1%c /= 'GHI' ) .or. ( g1%i /= 3001 ) .or. ( g1%j /= 3002 ) ) error stop 9_4
   close ( 1, status ='delete')

end program
