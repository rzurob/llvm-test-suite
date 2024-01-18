!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Section 4.5.4 Type Bound Procedures
!*                               (generic-binding)
!*                               - Specific Binding
!*                               - deferred specific type bound procedure
!*                               - deferred binding in two parent types,
!*                                 and implemented in gen3 type
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

   type, abstract :: base
      character(3) :: c = 'xxx'
      contains
         procedure(wbinf), deferred, pass :: write
         procedure(rbinf), deferred, pass :: read
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   type, abstract, extends(base) :: child
      integer(4) :: i = -999
      contains
         procedure(wcinf), deferred, pass :: write
         procedure(rcinf), deferred, pass :: read
   end type

   type, extends(child) :: gen3
      integer(4) :: j = -999
      contains
         procedure, pass :: write => writeg
         procedure, pass :: read  => readg
   end type

   abstract interface
      subroutine wbinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   abstract interface
      subroutine rbinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   abstract interface
      subroutine wcinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   abstract interface
      subroutine rcinf (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   contains

      subroutine writeg (dtv, unit, iotype, v_list, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4,1X,I4)", iostat=iostat, iomsg=iomsg)   dtv%c, dtv%i, dtv%j
         iomsg = 'dtiowriteg'

      end subroutine

      subroutine readg (dtv, unit, iotype, v_list, iostat, iomsg)
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

program abstracti007
   use m

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable  :: b1
   class(child), pointer     :: c1
   class(gen3), pointer      :: g1

   type(gen3)                :: g2
   type(gen3), parameter     :: g3 = gen3('mno',501,502)

   open ( 1, file = 'abstracti007.1', form='formatted', access='sequential' )

   allocate ( b1, source = gen3('abc',101, 102))
   allocate ( c1, source = gen3('def',201, 202))
   allocate ( g1, source = gen3('ghi',301, 302))

   g2 = gen3('jkl',401,402)

   write ( 1, *, iostat = stat, iomsg = msg )  b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 1_4

   write ( 1, *, iostat = stat, iomsg = msg )  g2, g3
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) ) error stop 2_4

   rewind 1

   deallocate ( b1, c1, g1 )
   allocate ( gen3 :: b1, c1, g1 )

   read ( 1, *, iostat = stat, iomsg = msg )  b1, c1, g1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) ) error stop 3_4

   read ( 1, *, iostat = stat, iomsg = msg )  g2, g2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadg' ) ) error stop 4_4

   select type ( b1 )
      type is ( gen3 )
         select type ( c1 )
            type is ( gen3 )
               if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 101 ) .or. ( b1%j /= 102 ) .or. &
                    ( c1%c /= 'def' ) .or. ( c1%i /= 201 ) .or. ( c1%j /= 202 ) .or. &
                    ( g1%c /= 'ghi' ) .or. ( g1%i /= 301 ) .or. ( g1%j /= 302 ) .or. &
                    ( g2%c /= 'mno' ) .or. ( g2%i /= 501 ) .or. ( g2%j /= 502 ) ) error stop 5_4
         end select
   end select

   close (1, status = 'delete' )

end program abstracti007
