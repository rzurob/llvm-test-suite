!*  ===================================================================
!*
!*  DATE                       : 2007-08-08 (original: 04/26/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Component
!*                                    -  polymorphic structure array component with formatted i/o
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

   type base (lbase1) ! lbase1=3
      integer, len :: lbase1
      character(lbase1) :: c = 'xxx'
      contains
         procedure, pass :: write => writeb
         procedure, pass :: read => readb
         generic :: write(formatted) => write
         generic :: read(formatted) => read
   end type

   type, extends(base) :: child (kchild1) ! kchild1=4
      integer, kind :: kchild1
      integer(kchild1) :: i = -999
      contains
         procedure, pass :: write => writec
         procedure, pass :: read => readc
   end type

   type container
      class(base(:)), allocatable :: b1(:) ! tcx: (:)
   end type

   contains

      subroutine writeb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtiowriteb'

      end subroutine

      subroutine readb (dtv, unit, iotype, v_list, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ")"
         read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c

         iomsg = 'dtioreadb'

      end subroutine

      subroutine writec (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"
         write (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtiowritec'

      end subroutine


      subroutine readc (dtv, unit, iotype, v_list, iostat, iomsg)
         class(child(*,4)), intent(inout) :: dtv ! tcx: (*,4)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(20) :: fmt

         write (fmt, *) "(A", v_list(1), ",1X, I",v_list(2),")"
         read (unit, fmt, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i

         iomsg = 'dtioreadc'

      end subroutine

end module

program structCompnt005kl
   use m

   integer :: stat
   character(200) :: msg

   type(container) :: cc1
   class(container), allocatable :: cc2

   open ( 1, file = 'structCompnt005kl.1', form='formatted', access='sequential' )

   cc1 = container( (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( cc2, source = container( (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   write ( 1, "(3(DT(3)))", iostat = stat, iomsg = msg )      cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, "(4(DT(3)))", iostat = stat, iomsg = msg )      cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 2_4

   deallocate ( cc2 )
   cc1 = container( (/ child(3,4)('jkl', 1001), child(3,4)('mno', 10002), child(3,4)('pqr', 100003) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( cc2, source = container( (/ child(3,4)('MNO', 21), child(3,4)('PQR', 202), child(3,4)('STU', 2003), child(3,4)('VWX', 20004) /) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   write ( 1, "(DT(3,4), DT(3,5), DT(3,6))", iostat = stat, iomsg = msg )    cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )                         error stop 3_4

   write ( 1, "(DT(3,2), DT(3,3), DT(3,4), DT(3,5))", iostat = stat, iomsg = msg )   cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )                                 error stop 4_4

   rewind 1

   deallocate ( cc2 )
   cc1 = container( (/ ( base(3)('xxx') , i=1,3 )  /) ) ! tcx: (3)
   allocate ( cc2, source = container( (/ ( base(3)('xxx') , i=1,4 )  /) ) ) ! tcx: (3)

   read ( 1, "(3(DT(3)))", iostat = stat, iomsg = msg )      cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 3_4

   if ( ( cc1%b1(1)%c /= 'abc' ) .or. ( cc1%b1(2)%c /= 'def' ) .or. ( cc1%b1(3)%c /= 'ghi' ) )    error stop 4_4

   read ( 1, "(4(DT(3)))",  iostat = stat, iomsg = msg )     cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) )          error stop 5_4

   if ( ( cc2%b1(1)%c /= 'ABC' ) .or. ( cc2%b1(2)%c /= 'DEF' ) .or. ( cc2%b1(3)%c /= 'GHI' ) .or. ( cc2%b1(4)%c /= 'JKL' ) )  error stop 6_4

   deallocate ( cc2 )
   cc1 = container( (/ ( child(3,4)('xxx',-999) , i=1,3 ) /) ) ! tcx: (3,4)
   allocate ( cc2, source = container( (/ ( child(3,4)('xxx',-999) , i=1,4 ) /) ) ) ! tcx: (3,4)

   read ( 1, "(DT(3,4), DT(3,5), DT(3,6))", iostat = stat, iomsg = msg )     cc1%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                         error stop 7_4

   select type ( g => cc1%b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
        if ( ( g(1)%c /= 'jkl' ) .or. ( g(1)%i /= 1001 ) .or. &
             ( g(2)%c /= 'mno' ) .or. ( g(2)%i /= 10002 ) .or. &
             ( g(3)%c /= 'pqr' ) .or. ( g(3)%i /= 100003 ) )                error stop 8_4
   end select

   read ( 1, "(DT(3,2), DT(3,3), DT(3,4), DT(3,5))", iostat = stat, iomsg = msg )    cc2%b1
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadc' ) )                                  error stop 9_4

   select type ( g => cc2%b1 )
      type is ( child(*,4) ) ! tcx: (*,4)
        if ( ( g(1)%c /= 'MNO' ) .or. ( g(1)%i /= 21 )  .or. &
             ( g(2)%c /= 'PQR' ) .or. ( g(2)%i /= 202 ) .or. &
             ( g(3)%c /= 'STU' ) .or. ( g(3)%i /= 2003 ) .or. &
             ( g(4)%c /= 'VWX' ) .or. ( g(4)%i /= 20004 ) )                 error stop 10_4
   end select

   close ( 1, status ='delete')

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase1) to invoke with (3) / declare with (*) - 12 changes
! type: child - added parameters (kchild1) to invoke with (4) / declare with (4) - 13 changes
! type: child - added parameters (kchild1) to invoke with (4) / declare with (4) - 13 changes
! type: child - added parameters (kchild1) to invoke with (3,4) / declare with (*,4) - 13 changes