! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - Scalar non-polymorphic type
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
      real(4) :: x
      real(4) :: y
   contains
      procedure, pass   :: getX
      procedure, pass   :: getY
      procedure, pass   :: setX
      procedure, pass   :: setY
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

   real(4) function getX(a)
      class(base), intent(in) :: a
      getX = a%x
   end function

   real(4) function getY(a)
      class(base), intent(in) :: a
      getY = a%y
   end function

   subroutine setX(a, i)
      class(base), intent(inout) :: a
      real(4) :: i
      a%x = i
   end subroutine

   subroutine setY(a, i)
      class(base), intent(inout) :: a
      real(4) :: i
      a%y = i
   end subroutine

end module

program scalar001
   use m1

   ! declaration of variables
   type(base), allocatable :: b1
   type(base), pointer     :: b2
   type(base)              :: b3

   logical :: precision_r4

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   ! allocation of variables

   allocate ( b1, source = base(x=1.0,y=2.0) )
   allocate ( b2, source = base(x=3.0,y=4.0) )
   b3       =    base(5.0, 6.0)

   ! I/O operations

   open ( u1%get(), file = 'scalar001.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2

   inquire ( iolength = length1 )  b1           !<- output list is a single scalar item

   if ( length1 /= 8 )             error stop 1_4

   inquire ( iolength = length1 )  b1, b2, b3   !<- output list is a scalar item list

   if ( length1 /= 24 )            error stop 2_4

   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b2, b3
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 3_4

   rewind u1%get()

   read ( u1%get(), iostat=stat1, iomsg=msg1 )       b3, b1, b2
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 4_4

   ! check if the values are written/read correctly

   if ( (.not. precision_r4(b3%getX(),1.0)) .or. (.not. precision_r4(b3%getY(),2.0)) ) error stop 5_4
   if ( (.not. precision_r4(b1%getX(),3.0)) .or. (.not. precision_r4(b1%getY(),4.0)) ) error stop 6_4
   if ( (.not. precision_r4(b2%getX(),5.0)) .or. (.not. precision_r4(b2%getY(),6.0)) ) error stop 7_4

   ! close the file appropriately

   close ( u1%get(), status ='delete' )

end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: length1
   real(4) :: tmpr1, tmpr2

   select type (dtv)
      type is (base)
         inquire ( iolength = length1 ) dtv
   end select

   if ( length1 /= 8 ) error stop 8_4

   read ( u1%get(), iostat=iostat, iomsg=iomsg ) tmpr1, tmpr2

   call dtv%setX(tmpr1)
   call dtv%setY(tmpr2)

end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base), intent(in) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: length1

   select type (dtv)
      type is (base)
         inquire ( iolength = length1 ) dtv
   end select

   if ( length1 /= 8 ) error stop 9_4

   write ( u1%get(), iostat=iostat, iomsg=iomsg ) dtv%getX(), dtv%getY()

end subroutine