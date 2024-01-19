!*  ===================================================================
!*
!*  DATE                       : 04/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

program specific007a
   use m1

   integer(4) :: stat
   character(200) :: msg

   class(base), allocatable :: b1(:)
   type(base)               :: b2(3) = (/ base('jkl'), base('mno'), base('pqr') /)
   class(base), pointer     :: c1(:)

   allocate ( b1(3), source = (/ base('abc'), base('def'), base('ghi') /) )
   allocate ( c1(3), source = (/ child('ABC', 1001), child('DEF', 1002), child('GHI', 1003) /) )

   open ( 1, file = 'specific007a.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )    error stop 2_4

   write (1, *, iostat=stat, iomsg = msg)  c1, child('JKL', 1004), child('MNO', 1005), child('PQR', 1006)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 3_4

   rewind 1

   read (1, *, iostat=stat, iomsg = msg)  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )     error stop 4_4

   read (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )     error stop 5_4

   read (1, *, iostat=stat, iomsg = msg)  c1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )     error stop 6_4

   select type ( c1 )
      type is ( child )
         if ( ( b1(1)%c /= 'jkl' ) .or. ( b1(2)%c /= 'mno' ) .or. ( b1(3)%c /= 'pqr' ) .or. &
              ( b2(1)%c /= 'abc' ) .or. ( b2(2)%c /= 'def' ) .or. ( b2(3)%c /= 'ghi' ) .or. &
              ( c1(1)%c /= 'JKL' ) .or. ( c1(2)%c /= 'MNO' ) .or. ( c1(3)%c /= 'PQR' ) .or. &
              ( c1(1)%i /= 1004  ) .or. ( c1(2)%i /= 1005  ) .or. ( c1(3)%i /= 1006  ) &
            ) error stop 7_4
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
