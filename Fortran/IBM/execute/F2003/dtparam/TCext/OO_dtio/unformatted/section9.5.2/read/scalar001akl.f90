! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar001akl
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Input Statement
!*                                        Try scalar entity with deferred-shape array components (Input)
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

program scalar001akl
   use m

   integer :: stat

   integer :: i1(2) = (/ 1,2 /)
   integer :: i2(3) = (/ 2,3,4 /)
   integer :: i3(4) = (/ 5,6,7,8 /)

   character(3) :: c1 = 'IBM'
   character(3) :: c2 = 'FTN'

   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,:)), pointer      :: b2 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   class(base(4,:)), pointer     :: b4 ! tcx: (4,:)

   open (1, file = 'scalar001akl.1', form='unformatted', access='sequential' )

   allocate(base(4,3):: b1, b2, b4) ! tcx: base(4,3)

   allocate(b1%b(1))
   allocate(b2%b(2))
   allocate(b3%b(3))
   allocate(b4%b(4))

   write (1, iostat=stat, iomsg=msg)       c1, i1(1)

   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i2

   write (1, iostat=stat, iomsg=msg)       c1, i3

   rewind 1

   read (1, iostat=stat, iomsg = msg)      b1
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )                    error stop 101_4
   if ( ( b1%c /= 'IBM' ) .or. ( b1%b(1)%i /= 1 ) )                   error stop 2_4

   read (1, iostat=stat, iomsg = msg)      b2, b3
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )                    error stop 3_4
   if ( ( b2%c /= 'IBM' ) .or. ( b2%b(1)%i /= 1 ) .or. ( b2%b(2)%i /= 2 ) .or. &
        ( b3%c /= 'FTN' ) .or. ( b3%b(1)%i /= 2 ) .or. ( b3%b(2)%i /= 3 ) .or. ( b3%b(3)%i /= 4 )) error stop 4_4

   read (1, iostat=stat, iomsg = msg)      b4
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )                    error stop 5_4
   if (( b4%c /= 'IBM' ) .or. ( b4%b(1)%i /= 5 ) .or. ( b4%b(2)%i /= 6 ) .or. ( b4%b(3)%i /= 7 ) .or. ( b4%b(4)%i /= 8 ) ) error stop 6_4


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
! type: mydata - added parameters (kmydata_1) to invoke with (4) / declare with (4) - 4 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (4,3) / declare with (4,*) - 6 changes
