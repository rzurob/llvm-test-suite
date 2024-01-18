!*  ===================================================================
!*
!*  DATE                       : 2007-08-09 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
      contains
         procedure, pass :: write => writebase
         procedure, pass :: read => readbase
         generic :: write(formatted) => write
         generic :: read(formatted)  => read
   end type

   contains

      subroutine writebase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, 9999
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readbase (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
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

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
      contains
         procedure, pass :: write => writechild
         procedure, pass :: read => readchild
   end type

   interface
      subroutine writechild (dtv, unit, iotype, v_list, iostat, iomsg)
         import child
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
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
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

end module

program specific007akl
   use m1

   integer(4) :: stat
   character(200) :: msg

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   type(base(3))               :: b2(3) = (/ base(3)('jkl'), base(3)('mno'), base(3)('pqr') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   class(base(:)), pointer     :: c1(:) ! tcx: (:)

   allocate ( b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( c1(3), source = (/ child(3,4)('ABC', 1001), child(3,4)('DEF', 1002), child(3,4)('GHI', 1003) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open ( 1, file = 'specific007akl.1', form='formatted', access='sequential' )

   write (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )    error stop 1_4

   write (1, *, iostat=stat, iomsg = msg)  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )    error stop 2_4

   write (1, *, iostat=stat, iomsg = msg)  c1, child(3,4)('JKL', 1004), child(3,4)('MNO', 1005), child(3,4)('PQR', 1006) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )    error stop 3_4

   rewind 1

   read (1, *, iostat=stat, iomsg = msg)  b2
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )     error stop 4_4

   read (1, *, iostat=stat, iomsg = msg)  b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )     error stop 5_4

   read (1, *, iostat=stat, iomsg = msg)  c1, c1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )     error stop 6_4

   select type ( c1 )
      type is ( child(*,4) ) ! tcx: (*,4)
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
   class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
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
   class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)  :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, "(A3,1X,I4)", iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
   iomsg = 'dtioreadc'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 11 changes
