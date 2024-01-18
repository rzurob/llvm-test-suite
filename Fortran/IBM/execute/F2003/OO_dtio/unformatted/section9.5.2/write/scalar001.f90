! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try scalar entity with explicit array components (Output)
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

   type :: mydata
      integer(4) ::  i
   end type

   type :: base
      type(mydata) :: b(2)
      character(3) :: c
   end type

   interface write(unformatted)
      subroutine writeunformatted(dtv, unit,iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine writeunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar001
   use m

   integer :: stat

   integer :: i1(2), i2(2)
   character(3) :: c1, c2

   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base), pointer      :: b2
   type(base)               :: b3
   class(base), pointer     :: b4

   type(mydata), allocatable :: d1
   type(mydata) :: d2
   type(mydata), allocatable :: d3(:)

   open (1, file = 'scalar001.1', form='unformatted', access='stream' )

   allocate (d1, source = mydata(777) )
   d2 = mydata(888)
   allocate (d3(2), source = (/d1, d2/) )

   allocate(b1, source = base(b=(/d2, d1/) , c='abc') )
   allocate(b2, source = base(b=(/d1, d2/) , c='def') )
   b3 =  base ( b = d3, c = 'ghi' )
   allocate(b4, source = base(b = d3 , c='def') )

   write (1, iostat=stat, iomsg=msg)       b1
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1, iostat=stat, iomsg=msg)       b2, b3
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1, iostat=stat, iomsg=msg)       b4
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   rewind 1

   read (1, iostat=stat, iomsg = msg)      c1, i1
   if ( ( c1 /= 'abc' ) .or. ( i1(1) /= 888 ) .or. ( i1(2) /= 777 ) ) error stop 4_4

   read (1, iostat=stat, iomsg = msg)      c1, i1, c2, i2
   if ( ( c1 /= 'def' ) .or. ( i1(1) /= 777 ) .or. ( i1(2) /= 888 ) .or. &
        ( c2 /= 'ghi' ) .or. ( i2(1) /= 777 ) .or. ( i2(2) /= 888 ) ) error stop 5_4

   read (1, iostat=stat, iomsg = msg)      c1, i1
   if ( ( c1 /= 'def' ) .or. ( i1(1) /= 777 ) .or. ( i1(2) /= 888 ) ) error stop 6_4

   close (1, status = 'delete' )

end program


subroutine writeunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, mydata

   interface write(unformatted)
      subroutine writeunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
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

   class(mydata), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )                   dtv%i

   iomsg = 'dtiowrite1'

end subroutine
