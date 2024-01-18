!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Input Statement
!*                                        Try array entity with explicit array components (Input)
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
      integer(4) ::  i = -999
   end type

   type :: base
      type(mydata) :: b(2)
      character(3) :: c = 'xxx'
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

program array002
   use m

   integer :: stat

   integer :: i1(2),i2(2),i3(2),i4(2)
   character(3) :: c1, c2, c3, c4

   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(base), pointer      :: b2(:)
   type(base)               :: b3(2,2)
   class(base), pointer     :: b4(:,:)

   c1 = 'abc'
   c2 = 'def'
   c3 = 'ghi'
   c4 = 'jkl'

   i1 = (/ 1, 2 /)
   i2 = (/ 3, 4 /)
   i3 = (/ 5, 6 /)
   i4 = (/ 7, 8 /)

   open (1, file = 'array002.1', form='unformatted', access='sequential' )

   allocate(b1(2), b2(3), b4(2,2))

   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i2
   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i2, c3, i3
   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i2, c3, i3, c4, i4
   write (1, iostat=stat, iomsg=msg)       c1, i1, c2, i2

   rewind 1

   read (1, iostat=stat, iomsg = msg)      b1(2:1:-1)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   if ( ( b1(1)%c /= 'def' ) .or. ( b1(1)%b(1)%i /= 3 ) .or. ( b1(1)%b(2)%i /= 4 ) .or. &
        ( b1(2)%c /= 'abc' ) .or. ( b1(2)%b(1)%i /= 1 ) .or. ( b1(2)%b(2)%i /= 2 ) ) error stop 2_4

   read (1, iostat=stat, iomsg = msg)      b2(1:3:2), b2(2)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   if ( ( b2(1)%c /= 'abc' ) .or. ( b2(1)%b(1)%i /= 1 ) .or. ( b2(1)%b(2)%i /= 2 ) .or. &
        ( b2(2)%c /= 'ghi' ) .or. ( b2(2)%b(1)%i /= 5 ) .or. ( b2(2)%b(2)%i /= 6 ) .or. &
        ( b2(3)%c /= 'def' ) .or. ( b2(3)%b(1)%i /= 3 ) .or. ( b2(3)%b(2)%i /= 4 ) ) error stop 4_4

   read (1, iostat=stat, iomsg = msg)      b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
   if ( ( b3(1,1)%c /= 'abc' ) .or. ( b3(1,1)%b(1)%i /= 1 ) .or. ( b3(1,1)%b(2)%i  /= 2 ) .or. &
        ( b3(2,1)%c /= 'def' ) .or. ( b3(2,1)%b(1)%i /= 3 ) .or. ( b3(2,1)%b(2)%i  /= 4 ) .or. &
        ( b3(1,2)%c /= 'ghi' ) .or. ( b3(1,2)%b(1)%i /= 5 ) .or. ( b3(1,2)%b(2)%i  /= 6 ) .or. &
        ( b3(2,2)%c /= 'jkl' ) .or. ( b3(2,2)%b(1)%i /= 7 ) .or. ( b3(2,2)%b(2)%i  /= 8 ) ) error stop 6_4

   read (1, iostat=stat, iomsg = msg)      b4(1,1:2)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 7_4
   if ( ( b4(1,1)%c /= 'abc' ) .or. ( b4(1,1)%b(1)%i /= 1 ) .or. ( b4(1,1)%b(2)%i /= 2 ) .or. &
        ( b4(1,2)%c /= 'def' ) .or. ( b4(1,2)%b(1)%i /= 3 ) .or. ( b4(1,2)%b(2)%i /= 4 ) ) error stop 8_4

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

   if ( ( iomsg /= 'dtioread1' ) .or. ( iostat /= 0 ) ) error stop 9_4

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
