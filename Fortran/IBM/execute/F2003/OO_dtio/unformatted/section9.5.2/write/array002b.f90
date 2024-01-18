! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try array entity with polymorphic components with class hierarchy (Output)
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
      integer(4) ::  i1 = 1
   end type

   type, extends(mydata) :: mysuperdata
      integer(4) ::  i2 = 2
   end type

   type :: base
      class(mydata), pointer :: b
   end type

   type, extends(base) :: child
      character(3) :: c = 'ibm'
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

program array002b
   use m

   integer :: i1(2), i2(2), i3(2), i4(2), i5(2), i6(2), i7(2), i8(2), i9(2), i10(2)
   character(3) :: c1, c2, c3, c4

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(base), pointer      :: b2(:)
   type(base)               :: b3(2,2)
   class(base), pointer     :: b4(:,:)

   open (1, file = 'array002b.1', form='unformatted', access='sequential' )

   allocate( b2(2))
   allocate ( child:: b1(2), b4(2,2) )
   allocate ( mysuperdata :: b1(1)%b, b1(2)%b, b2(1)%b, b2(2)%b &
              , b3(1,1)%b, b3(2,1)%b, b3(1,2)%b, b3(2,2)%b      &
              , b4(1,1)%b, b4(2,1)%b, b4(1,2)%b, b4(2,2)%b )

   select type (b1)
      type is (child)
         b1(1)%c= 'abc'
         b1(2)%c= 'def'
         associate ( g => b1(1)%b )
         select type (g)
            type is (mysuperdata)
               g%i1 = 101
               g%i2 = 102
         end select
         end associate
         associate ( g => b1(2)%b )
         select type (g)
            type is (mysuperdata)
               g%i1 = 103
               g%i2 = 104
         end select
         end associate
   end select

   associate ( g => b2(1)%b )
      select type (g)
         type is (mysuperdata)
            g%i1 = 105
            g%i2 = 106
      end select
   end associate
   associate ( g => b2(2)%b )
      select type (g)
         type is (mysuperdata)
            g%i1 = 107
            g%i2 = 108
      end select
   end associate

   b3 = reshape ( source = (/ b2, b2 /) , shape = (/2,2/) )

   select type (b4)
      type is (child)
         select type ( b1 )
            type is (child)
               b4 = reshape ( source = (/ b1, b1 /) , shape = (/2,2/) )
         end select
   end select

   write (1, iostat=stat, iomsg=msg)       b1(2:1:-1)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1, iostat=stat, iomsg=msg)       b2((/2,1/))
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1, iostat=stat, iomsg=msg)       b3
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write (1, iostat=stat, iomsg=msg)       b4(1,1:2)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   rewind 1

   read ( 1 ) i1, c1, i2, c2
   read ( 1 ) i3, i4
   read ( 1 ) i5, i6, i7, i8
   read ( 1 ) i9, c3, i10, c4

   print *, i1, c1, i2, c2
   print *, i3, i4
   print *, i5, i6, i7, i8
   print *, i9, c3, i10, c4

 !  close (1, status = 'delete' )

end program


subroutine writeunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, child, mydata

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

   write ( unit, iostat = iostat, iomsg = iomsg ) dtv%b
   if (( iomsg /= 'dtiowrite1' ) .or. ( iostat /= 0 ) ) error stop 5_4

   select type (dtv)
      type is (child)
         write ( unit, iostat = iostat ) dtv%c
         if (  iostat /= 0 ) error stop 6_4
   end select

   iomsg = 'dtiowrite'

end subroutine

subroutine writeunformatteddata (dtv, unit, iostat, iomsg)
   use m, only: mydata, mysuperdata

   class(mydata), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat )                   dtv%i1

   select type (dtv)
      type is (mysuperdata)
         write (unit, iostat=iostat)              dtv%i2
   end select

   iomsg = 'dtiowrite1'

end subroutine

