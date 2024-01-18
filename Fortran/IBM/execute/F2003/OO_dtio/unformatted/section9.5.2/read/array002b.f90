! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 Output Statement
!*                                        Try array entity with polymorphic components with class hierarchy (Input)
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
      integer(4) ::  i1 = -999
   end type

   type, extends(mydata) :: mysuperdata
      integer(4) ::  i2 = -9999
   end type

   type :: base
      class(mydata), pointer :: b
   end type

   type, extends(base) :: child
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

program array002b
   use m

   integer :: i1(2), i2(2), i3(2), i4(2), i5(2), i6(2), i7(2), i8(2), i9(2), i10(2), i11(2), i12(2)
   character(3) :: c1, c2, c3, c4, c5, c6

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(base), pointer      :: b2(:)
   type(base)               :: b3(2,2)
   class(base), pointer     :: b4(:,:)

   open (1, file = 'array002b.1', form='unformatted', access='sequential' )

   allocate( b2(2))
   allocate ( child :: b1(2), b4(2,2) )
   allocate ( mysuperdata :: b1(1)%b, b1(2)%b, b2(1)%b, b2(2)%b &
              , b3(1,1)%b, b3(2,1)%b, b3(1,2)%b, b3(2,2)%b      &
              , b4(1,1)%b, b4(2,1)%b, b4(1,2)%b, b4(2,2)%b )

   i1 = (/ 1, 2 /)
   c1 = 'abc'
   i2 = (/ 3, 4 /)
   c2 = 'def'
   i3 = (/ 5, 6 /)
   i4 = (/ 7, 8 /)
   i5 = (/ 9, 10 /)
   i6 = (/ 11, 12 /)
   i7 = (/ 13, 14 /)
   i8 = (/ 15, 16 /)
   i9 = (/ 17, 18 /)
   c3 = 'ghi'
   i10 = (/ 19, 20 /)
   c4 = 'jkl'
   i11 = (/ 21, 22 /)
   c5 = 'mno'
   i12 = (/ 23, 24 /)
   c6 = 'pqr'

   write ( 1 ) i1, c1, i2, c2
   write ( 1 ) i3, i4
   write ( 1 ) i5, i6, i7, i8
   write ( 1 ) i9, c3, i10, c4

   rewind 1

   read (1, iostat=stat, iomsg=msg)       b1(2:1:-1)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1, iostat=stat, iomsg=msg)       b2
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1, iostat=stat, iomsg=msg)       b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read (1, iostat=stat, iomsg=msg)       b4(1,1:2)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type (b1)
      type is (child)
         print *, b1(1)%c, b1(2)%c
         associate ( g => b1(1)%b )
         select type (g)
            type is (mysuperdata)
               print *, g%i1
               print *, g%i2
         end select
         end associate
         associate ( g => b1(2)%b )
         select type (g)
            type is (mysuperdata)
               print *, g%i1
               print *, g%i2
         end select
         end associate
   end select

   associate ( g => b2(1)%b )
      select type (g)
         type is (mysuperdata)
            print *, g%i1
            print *, g%i2
      end select
   end associate
   associate ( g => b2(2)%b )
      select type (g)
         type is (mysuperdata)
            print *, g%i1
            print *, g%i2
      end select
   end associate

   associate ( g => b3(1,1)%b )
      select type (g )
         type is (mysuperdata)
            print *, g%i1
            print *, g%i2
      end select
   end associate
   associate ( g => b3(2,1)%b )
      select type (g )
         type is (mysuperdata)
            print *, g%i1
            print *, g%i2
      end select
   end associate
   associate ( g => b3(1,2)%b )
      select type (g )
         type is (mysuperdata)
            print *, g%i1
            print *, g%i2
      end select
   end associate
   associate ( g => b3(2,2)%b )
      select type (g )
         type is (mysuperdata)
            print *, g%i1
            print *, g%i2
      end select
   end associate

   select type (b4)
      type is (child)
         print *, b4(1,1)%c, b4(2,1)%c, b4(1,2)%c, b4(2,2)%c
         associate ( g => b4(1,1)%b )
            select type (g )
               type is (mysuperdata)
                  print *, g%i1
                  print *, g%i2
            end select
         end associate
         associate ( g => b4(2,1)%b )
            select type (g )
               type is (mysuperdata)
                  print *, g%i1
                  print *, g%i2
            end select
         end associate
         associate ( g => b4(1,2)%b )
            select type (g )
               type is (mysuperdata)
                  print *, g%i1
                  print *, g%i2
            end select
         end associate
         associate ( g => b4(2,2)%b )
            select type (g )
               type is (mysuperdata)
                  print *, g%i1
                  print *, g%i2
            end select
         end associate
   end select


   close (1, status = 'delete' )

end program


subroutine readunformatted ( dtv, unit, iostat, iomsg )
   use m, only: base, child, mydata

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

   read ( unit, iostat = iostat, iomsg = iomsg ) dtv%b
   if ( ( iomsg /= 'dtioread1' ) .or. ( iostat /= 0 ) ) error stop 5_4

   select type (dtv)
      type is (child)
         read ( unit, iostat = iostat ) dtv%c
         if ( iostat /= 0  ) error stop 6_4
   end select

   iomsg = 'dtioread'

end subroutine

subroutine readunformatteddata (dtv, unit, iostat, iomsg)
   use m, only: mydata, mysuperdata

   class(mydata), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat )                   dtv%i1

   select type (dtv)
      type is (mysuperdata)
         read (unit, iostat=iostat)              dtv%i2
   end select

   iomsg = 'dtioread1'

end subroutine

