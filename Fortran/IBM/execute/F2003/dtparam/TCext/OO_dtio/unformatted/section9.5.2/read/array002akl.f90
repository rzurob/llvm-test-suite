! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array002akl
!*
!*  PROGRAMMER                 : David Forster (derived from array002a by Robert Ma)
!*  DATE                       : 2007-09-13 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Input Statement
!*                                        Try array entity with deferred array components (Input)
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
      integer(kmydata_1) ::  i = -999
   end type

   type :: base (kbase_1,lbase_1) ! kbase_1,lbase_1=4,3
      integer, kind :: kbase_1
      integer, len :: lbase_1
      class(mydata(kbase_1)), allocatable :: b(:) ! tcx: (kbase_1)
      character(lbase_1) :: c = 'xxx'
   end type

   interface read(unformatted)
      subroutine readunformatted(dtv, unit,iostat, iomsg )
         import base
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine readunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program array002akl
   use m

   integer :: stat

   integer :: i1(2), i2(2), i3(2), i4(3), i5(3), i6(3), i7(3), i8(4), i9(4)
   character(3) :: c1, c2, c3, c4

   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1(:) ! tcx: (4,:)
   type(base(4,:)), pointer      :: b2(:) ! tcx: (4,:)
   type(base(4,3))               :: b3(2,2) ! tcx: (4,3)
   class(base(4,:)), pointer     :: b4(:,:) ! tcx: (4,:)

   c1 = 'abc'
   c2 = 'def'
   c3 = 'ghi'
   c4 = 'jkl'

   i1 = (/1,2/)
   i2 = (/3,4/)
   i3 = (/5,6/)
   i4 = (/8,9,10/)
   i5 = (/11,12,13/)
   i6 = (/14,15,16/)
   i7 = (/17,18,19/)
   i8 = (/17,18,19,20/)
   i9 = (/21,22,23,24/)

   open (1, file = 'array002akl.1', form='unformatted', access='sequential' )

   allocate(base(4,3):: b1(2), b2(3), b4(2,2) ) ! tcx: base(4,3)

   allocate( b1(1)%b(1), b1(2)%b(1),b2(1)%b(2),b2(2)%b(2),b2(3)%b(2),b3(1,1)%b(3), &
             b3(2,1)%b(3), b3(1,2)%b(3), b3(2,2)%b(3), b4(1,1)%b(4), b4(2,1)%b(4), &
             b4(1,2)%b(4),  b4(2,2)%b(4) )

   write (1, iostat=stat, iomsg=msg)       c1, i1(1), c2, i2(1)

   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i2, c3, i3

   write (1, iostat=stat, iomsg=msg)       c1, i4, c2, i5, c3, i6, c4, i7

   write (1, iostat=stat, iomsg=msg)       c1, i8, c2, i9

   rewind 1

   read (1, iostat=stat, iomsg = msg)      b1(2:1:-1)
   if ( ( b1(1)%c /= 'def' ) .or. ( b1(1)%b(1)%i /= 3 ) .or. &
        ( b1(2)%c /= 'abc' ) .or. ( b1(2)%b(1)%i /= 1 ) ) error stop 2_4

   read (1, iostat=stat, iomsg = msg)      b2(1:3:2), b2(2)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   if ( ( b2(1)%c /= 'abc' ) .or. ( b2(1)%b(1)%i /= 1 ) .or. ( b2(1)%b(2)%i /= 2 ) .or. &
        ( b2(2)%c /= 'ghi' ) .or. ( b2(2)%b(1)%i /= 5 ) .or. ( b2(2)%b(2)%i /= 6 ) .or. &
        ( b2(3)%c /= 'def' ) .or. ( b2(3)%b(1)%i /= 3 ) .or. ( b2(3)%b(2)%i /= 4 ) ) error stop 4_4

   read (1, iostat=stat, iomsg = msg)      b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   if ( ( b3(1,1)%c /= 'abc' ) .or. ( b3(1,1)%b(1)%i /= 8 ) .or. ( b3(1,1)%b(2)%i  /= 9 ) .or. ( b3(1,1)%b(3)%i  /= 10 ).or. &
        ( b3(2,1)%c /= 'def' ) .or. ( b3(2,1)%b(1)%i /= 11 ) .or. ( b3(2,1)%b(2)%i  /= 12 ) .or. ( b3(2,1)%b(3)%i  /= 13 ).or. &
        ( b3(1,2)%c /= 'ghi' ) .or. ( b3(1,2)%b(1)%i /= 14 ) .or. ( b3(1,2)%b(2)%i  /= 15 ) .or. ( b3(1,2)%b(3)%i  /= 16 ).or. &
        ( b3(2,2)%c /= 'jkl' ) .or. ( b3(2,2)%b(1)%i /= 17 ) .or. ( b3(2,2)%b(2)%i  /= 18 ) .or. ( b3(2,2)%b(3)%i  /= 19 )) error stop 6_4

   read (1, iostat=stat, iomsg = msg)      b4(1,1:2)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 7_4
   if ( ( b4(1,1)%c /= 'abc' ) .or. ( b4(1,1)%b(1)%i /= 17 ) .or. ( b4(1,1)%b(2)%i /= 18 ) .or. ( b4(1,1)%b(3)%i /= 19 ) .or. ( b4(1,1)%b(4)%i /= 20 ) .or. &
        ( b4(1,2)%c /= 'def' ) .or. ( b4(1,2)%b(1)%i /= 21 ) .or. ( b4(1,2)%b(2)%i /= 22 ) .or. ( b4(1,2)%b(3)%i /= 23 ) .or. ( b4(1,2)%b(4)%i /= 24 )) error stop 8_4

   close (1, status = 'delete' )

end program


subroutine readunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, mydata

   interface read(unformatted)
      subroutine readunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
        class(mydata(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )                  dtv%c

   read (unit, iostat=iostat, iomsg = iomsg )   dtv%b

   if ( ( iomsg /= 'dtioread1' ) .or. ( iostat /= 0 ) ) error stop 9_4

   iomsg = 'dtioread'

end subroutine

subroutine readunformatteddata (dtv, unit, iostat, iomsg)
   use m, only: mydata

   class(mydata(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )                   dtv%i

   iomsg = 'dtioread1'

end subroutine


! Extensions to introduce derived type parameters:
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 14 changes
! type: mydata - added parameters (kmydata_1) to invoke with (4) / declare with (4) - 4 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (4,3) / declare with (4,*) - 6 changes
