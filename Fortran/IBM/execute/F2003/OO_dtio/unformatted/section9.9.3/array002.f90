!#######################################################################
! OO_dtio/unformatted/section9.9.3/array002.f, xlftest.OO_dtio, tstdev, 1.1
! Extract Date/Time: 05/04/06 10:42:37
! Checkin Date/Time: 04/12/15 16:01:17
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
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - inquire iolength of array polymorphic items
!*                                 when polymorphic items are in "type is" type guard
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type :: base
      integer(8)   :: x
      real(4)      :: y
      character(3) :: z
   contains
      procedure, pass   :: getX
      procedure, pass   :: getY
      procedure, pass   :: getZ
      procedure, pass   :: setX
      procedure, pass   :: setY
      procedure, pass   :: setZ
   end type

   type, extends(base) :: child
      integer(2) :: a
      real(4)    :: b
      complex(4) :: c
   end type

   type :: unit
      integer(4) :: unumber
   contains
      procedure, pass :: get
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   type(unit) :: u1 = unit(2)

contains

   integer(4) function get(a)
      class(unit), intent(in) :: a
      get = a%unumber
   end function

   integer(8) function getX(a)
      class(base), intent(in) :: a
      getX = a%x
   end function

   real(4) function getY(a)
      class(base), intent(in) :: a
      getY = a%y
   end function

   character(3) function getZ(a)
      class(base), intent(in) :: a
      getZ = a%z
   end function

   subroutine setX(a, i)
      class(base), intent(inout) :: a
      integer(8) :: i
      a%x = i
   end subroutine

   subroutine setY(a, i)
      class(base), intent(inout) :: a
      real(4) :: i
      a%y = i
   end subroutine

   subroutine setZ(a, i)
      class(base), intent(inout) :: a
      character(3) :: i
      a%z = i
   end subroutine

end module

program array002
   use m1

   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   class(base), allocatable :: b3(:)
   class(base), pointer     :: b4(:,:)

   logical :: precision_r4

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   ! allocation of variables

   allocate ( b1(3), source = (/ (base(x=1,y=2.0,z='ibm'), i=1,3 ) /) )
   allocate ( b2(2,2), source = reshape ( source=(/(child(x=3,y=4.0,z='ftn', a=5, b=6.0, c=(7.0,8.0)),i=1,4)/), shape= (/2,2/) ))
   allocate ( b3(3) )
   allocate ( child :: b4(2,2) )

   ! I/O operations

   open ( u1%get(), file = 'array002.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2

   select type (b1)
      type is (base)
         inquire ( iolength = length1 )   b1           !<- output list is a single scalar item
      class default
         error stop 1_4
   end select

   if ( length1 /= 48 )             error stop 2_4

   select type (b2)
      type is (base)
         error stop 3_4
      type is (child)
         inquire ( iolength = length1 )   b2           !<- output list is a single scalar item
      class default
         error stop 4_4
   end select

   if ( length1 /= 128 )             error stop 5_4

   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b2

   rewind u1%get()

   read  ( u1%get(), iostat=stat1, iomsg=msg1 )      b3, b4

   ! check if values are read correctly

      ! check if the values are read/written correctly

   if ( ( b3(1)%getX() /= 1 ) .or. (.not. precision_r4(b3(1)%getY(),2.0)) .or. (b3(1)%getZ() /= 'ibm') .or. &
        ( b3(2)%getX() /= 1 ) .or. (.not. precision_r4(b3(2)%getY(),2.0)) .or. (b3(2)%getZ() /= 'ibm') .or. &
        ( b3(3)%getX() /= 1 ) .or. (.not. precision_r4(b3(3)%getY(),2.0)) .or. (b3(3)%getZ() /= 'ibm') )            error stop 8_4

   select type (b4)
      type is (child)
         if ( ( b4(1,1)%getX() /= 3 ) .or. (.not. precision_r4(b4(1,1)%getY(),4.0)) .or. (b4(1,1)%getZ() /= 'ftn')        .or. &
              ( b4(1,1)%a /= 5 ) .or. (.not. precision_r4(b4(1,1)%b,6.0)) .or. (.not. precision_r4(b4(1,1)%c, (7.0,8.0))) .or. &
              ( b4(2,1)%getX() /= 3 ) .or. (.not. precision_r4(b4(2,1)%getY(),4.0)) .or. (b4(2,1)%getZ() /= 'ftn')        .or. &
              ( b4(2,1)%a /= 5 ) .or. (.not. precision_r4(b4(2,1)%b,6.0)) .or. (.not. precision_r4(b4(2,1)%c, (7.0,8.0))) .or. &
              ( b4(1,2)%getX() /= 3 ) .or. (.not. precision_r4(b4(1,2)%getY(),4.0)) .or. (b4(1,2)%getZ() /= 'ftn')        .or. &
              ( b4(1,2)%a /= 5 ) .or. (.not. precision_r4(b4(1,2)%b,6.0)) .or. (.not. precision_r4(b4(1,2)%c, (7.0,8.0))) .or. &
              ( b4(2,2)%getX() /= 3 ) .or. (.not. precision_r4(b4(2,2)%getY(),4.0)) .or. (b4(2,2)%getZ() /= 'ftn')        .or. &
              ( b4(2,2)%a /= 5 ) .or. (.not. precision_r4(b4(2,2)%b,6.0)) .or. (.not. precision_r4(b4(2,2)%c, (7.0,8.0))) &
               )      error stop 8_4
      class default
         error stop 9_4
   end select

   ! close the file appropriately

   close ( u1%get(), status ='delete' )

end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: length1

   select type ( a => dtv )
      type is (base)
         inquire ( iolength = length1 ) a
         if ( length1 /= 16 )    error stop 10_4
         read ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z
      type is (child)
         inquire ( iolength = length1 ) a
         if ( length1 /= 32 )    error stop 11_4
         read ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z, a%a, a%b, a%c
      class default
         error stop 12_4
   end select

end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base), intent(in) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( a => dtv )
      type is (base)
         inquire ( iolength = length1 ) a
         if ( length1 /= 16 )    error stop 13_4
                               write ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z
      type is (child)
         inquire ( iolength = length1 ) a
         if ( length1 /= 32 )    error stop 14_4
         write ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z, a%a, a%b, a%c
      class default
         error stop 15_4
   end select

end subroutine
