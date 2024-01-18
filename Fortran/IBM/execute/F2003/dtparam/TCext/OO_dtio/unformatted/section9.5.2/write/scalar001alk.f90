! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try scalar entity with deferred-shape array components (Output)
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

   type :: mydata (kmydata_1) ! kmydata_1=4
      integer, kind :: kmydata_1
      integer(kmydata_1) ::  i
   end type

   type :: base (kbase_1,lbase_1) ! kbase_1,lbase_1=4,3
      integer, kind :: kbase_1
      integer, len :: lbase_1
      class(mydata(kbase_1)), allocatable :: b(:) ! tcx: (kbase_1)
      character(lbase_1) :: c
   end type

   interface write(unformatted)
      subroutine writeunformatted(dtv, unit,iostat, iomsg )
         import base
         class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine writeunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar001alk
   use m

   integer :: stat

   integer :: i1(2), i2(3), i3(4)
   character(3) :: c1, c2

   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,:)), pointer      :: b2 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   class(base(4,:)), pointer     :: b4 ! tcx: (4,:)

   class(mydata(4)), allocatable :: d1 ! tcx: (4)
   type(mydata(4)) :: d2 ! tcx: (4)
   class(mydata(4)), pointer :: d3(:) ! tcx: (4)

   open (1, file = 'scalar001alk.1', form='unformatted', access='sequential' )

   allocate (d1, source = mydata(4)(777) ) ! tcx: (4)
   d2 = mydata(4)(888) ! tcx: (4)
   allocate (d3(2), source = (/d1, d2/) )

   allocate(b1, source = base(4,3)(b=null() , c='abc') ) ! tcx: (4,3)
   allocate(b2, source = base(4,3)(b=null() , c='def') ) ! tcx: (4,3)
   b3 =  base(4,3) ( b = null(), c = 'ghi' ) ! tcx: (4,3)
   allocate(b4, source = base(4,3)(b = null() , c='def') ) ! tcx: (4,3)

   allocate(b1%b(1), source = d1 )
   allocate(b2%b(2), source = d3 )
   allocate(b3%b(3), source = (/ d1, d3 /) )
   allocate(b4%b(4), source = (/ d1, d2, d3 /) )

   write (1, iostat=stat, iomsg=msg)       b1
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) )                   error stop 101_4

   write (1, iostat=stat, iomsg=msg)       b2, b3
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) )                   error stop 2_4

   write (1, iostat=stat, iomsg=msg)       b4
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) )                   error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1, i1(1)
   if ( ( c1 /= 'abc' ) .or. ( i1(1) /= 777 ) )                       error stop 4_4

   read (1, iostat=stat, iomsg = msg)      c1, i1, c2, i2
   if ( ( c1 /= 'def' ) .or. ( i1(1) /= 777 ) .or. ( i1(2) /= 888 ) .or. &
        ( c2 /= 'ghi' ) .or. ( i2(1) /= 777 ) .or. ( i2(2) /= 777 ) .or. ( i2(3) /= 888 )) error stop 5_4

   read (1, iostat=stat, iomsg = msg)      c1, i3
   if ( ( c1 /= 'def' ) .or. ( i1(1) /= 777 ) .or. ( i1(2) /= 888 ) .or. ( i1(1) /= 777 ) .or. ( i1(2) /= 888 ) ) error stop 6_4


   close (1, status = 'delete' )
end program


subroutine writeunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, mydata

   interface write(unformatted)
      subroutine writeunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )                  dtv%c

   write (unit, iostat=iostat, iomsg = iomsg )   dtv%b

   if ( ( iomsg /= 'dtiowrite1' ) .or. ( iostat /= 0 ) ) error stop 7_4

   iomsg = 'dtiowrite'

end subroutine

subroutine writeunformatteddata (dtv, unit, iostat, iomsg)
   use m, only: mydata

   class(mydata(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )                   dtv%i

   iomsg = 'dtiowrite1'

end subroutine


! Extensions to introduce derived type parameters:
! type: mydata - added parameters (kmydata_1) to invoke with (4) / declare with (4) - 9 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (4,3) / declare with (4,*) - 10 changes
