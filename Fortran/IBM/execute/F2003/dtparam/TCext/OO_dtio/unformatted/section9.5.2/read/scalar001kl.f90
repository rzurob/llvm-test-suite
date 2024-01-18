! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar001kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar001 by Robert Ma)
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
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
!*                                        Try scalar entity with explicit array components (input)
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

   interface read(unformatted)
      subroutine readunformatted(dtv, unit,iostat, iomsg )
         import base
         class(base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
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

program scalar001kl
   use m

   integer :: stat

   integer :: i1(2), i2(2)
   character(3) :: c1, c2

   character(200) :: msg = ''
   class(base(4,:,:)), allocatable :: b1 ! tcx: (4,:,:)
   type(base(4,:,:)), pointer      :: b2 ! tcx: (4,:,:)
   type(base(4,3,2))               :: b3 ! tcx: (4,3,2)
   class(base(4,:,:)), pointer     :: b4 ! tcx: (4,:,:)

   type(mydata(4)), allocatable :: d1 ! tcx: (4)
   type(mydata(4)) :: d2 ! tcx: (4)
   type(mydata(4)), allocatable :: d3(:) ! tcx: (4)

   open (1, file = 'scalar001kl.1', form='unformatted', access='stream' )

   allocate (d1, source = mydata(4)(999) ) ! tcx: (4)
   d2 = mydata(4)(999) ! tcx: (4)
   allocate (d3(2), source = (/d1, d2/) )

   allocate(b1, source = base(4,3,2)(b=(/d2, d1/) , c='xxx') ) ! tcx: (4,3,2)
   allocate(b2, source = base(4,3,2)(b=(/d1, d2/) , c='xxx') ) ! tcx: (4,3,2)
   b3 =  base(4,3,2) ( b = d3, c = 'xxx' ) ! tcx: (4,3,2)
   allocate(b4, source = base(4,3,2)(b = d3 , c='xxx') ) ! tcx: (4,3,2)

   c1 = 'abc'
   c2 = 'def'
   i1 = (/ 777, 888 /)

   write (1, iostat=stat, iomsg=msg)       c1, i1

   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i1

   write (1, iostat=stat, iomsg=msg)       c1, i1


   rewind 1

   read (1, iostat=stat, iomsg = msg)      b1
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 101_4
   if ( ( b1%c /= 'abc' ) .or. ( b1%b(1)%i /= 777 ) .or. ( b1%b(2)%i /= 888 ) ) error stop 4_4

   read (1, iostat=stat, iomsg = msg)      b2, b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   if ( ( b2%c /= 'abc' ) .or. ( b2%b(1)%i /= 777 ) .or. ( b2%b(2)%i /= 888 ) .or. &
        ( b3%c /= 'def' ) .or. ( b3%b(1)%i /= 777 ) .or. ( b3%b(2)%i /= 888 ) ) error stop 5_4

   read (1, iostat=stat, iomsg = msg)      b4
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   if ( ( b4%c /= 'abc' ) .or. ( b4%b(1)%i /= 777 ) .or. ( b4%b(2)%i /= 888 ) ) error stop 6_4

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

   class(base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )                  dtv%c

   read (unit, iostat=iostat, iomsg = iomsg )   dtv%b

   if ( ( iomsg /= 'dtioread1' ) .or. ( iostat /= 0 ) ) error stop 7_4

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
! type: mydata - added parameters (kmydata_1) to invoke with (4) / declare with (4) - 9 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,3,2) / declare with (4,*,*) - 10 changes
