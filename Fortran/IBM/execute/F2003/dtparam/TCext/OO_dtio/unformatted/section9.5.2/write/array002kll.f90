! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array002kll
!*
!*  PROGRAMMER                 : David Forster (derived from array002 by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try array entity with explicit array components (Output)
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

   type :: base (kbase_1,lbase_1,lbase_2) ! kbase_1,lbase_1,lbase_2=4,3,2
      integer, kind :: kbase_1
      integer, len :: lbase_1,lbase_2
      type(mydata(kbase_1)) :: b(lbase_2) ! tcx: (kbase_1)
      character(lbase_1) :: c
   end type

   interface write(unformatted)
      subroutine writeunformatted(dtv, unit,iostat, iomsg )
         import base
         class(base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
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

program array002kll
   use m

   integer :: stat

   integer :: i1(2), i2(2), i3(2), i4(2)
   character(3) :: c1, c2, c3, c4

   character(200) :: msg = ''
   class(base(4,:,:)), allocatable :: b1(:) ! tcx: (4,:,:)
   type(base(4,:,:)), pointer      :: b2(:) ! tcx: (4,:,:)
   type(base(4,3,2))               :: b3(2,2) ! tcx: (4,3,2)
   class(base(4,:,:)), pointer     :: b4(:,:) ! tcx: (4,:,:)

   type(mydata(4)), allocatable :: d1 ! tcx: (4)
   type(mydata(4)) :: d2 ! tcx: (4)
   type(mydata(4)), allocatable :: d3(:) ! tcx: (4)

   open (1, file = 'array002kll.1', form='unformatted', access='sequential' )

   allocate (d1, source = mydata(4)(111) ) ! tcx: (4)
   d2 = mydata(4)(222) ! tcx: (4)
   allocate (d3(2), source = (/d1, d2/) )

   allocate(b1(2), source = (/ base(4,3,2)(b=(/d1, d2/) , c='abc') , base(4,3,2)(b=(/d1, d2/) , c='def') /)) ! tcx: (4,3,2) ! tcx: (4,3,2)
   allocate(b2(3), source = (/ b1 , base(4,3,2)(b=(/d1, d2/) , c='ghi') /) ) ! tcx: (4,3,2)
   b3 =  reshape ( source = (/ base(4,3,2)(b=(/d1, d2/) , c='abc') , base(4,3,2)(b=(/d1, d2/) , c='def'), & ! tcx: (4,3,2) ! tcx: (4,3,2)
                               base(4,3,2)(b=(/d1, d2/) , c='ghi') , base(4,3,2)(b=(/d1, d2/) , c='jkl') /), shape = (/2,2/) ) ! tcx: (4,3,2) ! tcx: (4,3,2)
   allocate( b4(2,2), source = b3 )

   write (1, iostat=stat, iomsg=msg)       b1(2:1:-1)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4

   write (1, iostat=stat, iomsg=msg)       b2((/3,1,2/))
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1, iostat=stat, iomsg=msg)       b3
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write (1, iostat=stat, iomsg=msg)       b4(1,1:2)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1, i1, c2, i2
   if ( ( c1 /= 'def' ) .or. ( i1(1) /= 111 ) .or. ( i1(2) /= 222 ) .or. &
        ( c2 /= 'abc' ) .or. ( i2(1) /= 111 ) .or. ( i2(2) /= 222 ) ) error stop 5_4

   read (1, iostat=stat, iomsg = msg)      c1, i1, c2, i2, c3, i3
   if ( ( c1 /= 'ghi' ) .or. ( i1(1) /= 111 ) .or. ( i1(2) /= 222 ) .or. &
        ( c2 /= 'abc' ) .or. ( i2(1) /= 111 ) .or. ( i2(2) /= 222 ) .or. &
        ( c3 /= 'def' ) .or. ( i3(1) /= 111 ) .or. ( i3(2) /= 222 ) ) error stop 6_4

   read (1, iostat=stat, iomsg = msg)      c1, i1, c2, i2, c3, i3, c4, i4
   if ( ( c1 /= 'abc' ) .or. ( i1(1) /= 111 ) .or. ( i1(2) /= 222 ) .or. &
        ( c2 /= 'def' ) .or. ( i2(1) /= 111 ) .or. ( i2(2) /= 222 ) .or. &
        ( c3 /= 'ghi' ) .or. ( i3(1) /= 111 ) .or. ( i3(2) /= 222 ) .or. &
        ( c4 /= 'jkl' ) .or. ( i4(1) /= 111 ) .or. ( i4(2) /= 222 ) ) error stop 7_4

   read (1, iostat=stat, iomsg = msg)      c1, i1, c2, i2
   if ( ( c1 /= 'abc' ) .or. ( i1(1) /= 111 ) .or. ( i1(2) /= 222 ) .or. &
        ( c2 /= 'ghi' ) .or. ( i2(1) /= 111 ) .or. ( i2(2) /= 222 ) ) error stop 8_4

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

   class(base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )                  dtv%c

   write (unit, iostat=iostat, iomsg = iomsg )   dtv%b

   if ( ( iomsg /= 'dtiowrite1' ) .or. ( iostat /= 0 ) ) error stop 9_4

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
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,3,2) / declare with (4,*,*) - 13 changes
