! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

   type :: mydata
      integer(4) ::  i
   end type

   type :: base
      type(mydata) :: b(2)
      character(3) :: c
   end type

   interface read(unformatted)
      subroutine readunformatted(dtv, unit,iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine readunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata), intent(inout) :: dtv
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

   allocate (d1, source = mydata(999) )
   d2 = mydata(999)
   allocate (d3(2), source = (/d1, d2/) )

   allocate(b1, source = base(b=(/d2, d1/) , c='xxx') )
   allocate(b2, source = base(b=(/d1, d2/) , c='xxx') )
   b3 =  base ( b = d3, c = 'xxx' )
   allocate(b4, source = base(b = d3 , c='xxx') )

   c1 = 'abc'
   c2 = 'def'
   i1 = (/ 777, 888 /)

   write (1, iostat=stat, iomsg=msg)       c1, i1

   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i1

   write (1, iostat=stat, iomsg=msg)       c1, i1


   rewind 1

   read (1, iostat=stat, iomsg = msg)      b1
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   if ( ( b1%c /= 'abc' ) .or. ( b1%b(1)%i /= 777 ) .or. ( b1%b(2)%i /= 888 ) ) error stop 4_4

   read (1, iostat=stat, iomsg = msg)      b2, b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   if ( ( b2%c /= 'abc' ) .or. ( b2%b(1)%i /= 777 ) .or. ( b2%b(2)%i /= 888 ) .or. &
        ( b3%c /= 'def' ) .or. ( b3%b(1)%i /= 777 ) .or. ( b3%b(2)%i /= 888 ) ) error stop 5_4

   read (1, iostat=stat, iomsg = msg)      b4
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   if ( ( b4%c /= 'abc' ) .or. ( b4%b(1)%i /= 777 ) .or. ( b4%b(2)%i /= 888 ) ) error stop 6_4

   close (1, status = 'delete' )

end program


subroutine readunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, mydata

   interface read(unformatted)
      subroutine readunformatteddata(dtv, unit, iostat, iomsg )
         import mydata
         class(mydata), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(inout) :: dtv
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

   class(mydata), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )                   dtv%i

   iomsg = 'dtioread1'

end subroutine
