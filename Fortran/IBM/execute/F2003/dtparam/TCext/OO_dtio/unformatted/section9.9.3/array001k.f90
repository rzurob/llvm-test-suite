! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001k
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - array non-polymorphic type
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
   type :: base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      real(kbase_1) :: x
      real(kbase_1) :: y
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
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
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
      class(base(4)), intent(in) :: a ! tcx: (4)
      getX = a%x
   end function

   real(4) function getY(a)
      class(base(4)), intent(in) :: a ! tcx: (4)
      getY = a%y
   end function

   subroutine setX(a, i)
      class(base(4)), intent(inout) :: a ! tcx: (4)
      real(4) :: i
      a%x = i
   end subroutine

   subroutine setY(a, i)
      class(base(4)), intent(inout) :: a ! tcx: (4)
      real(4) :: i
      a%y = i
   end subroutine

end module

program array001k
   use m1

   ! declaration of variables
   type(base(4)), allocatable :: b1(:) ! tcx: (4)
   type(base(4)), pointer     :: b2(:,:) ! tcx: (4)
   type(base(4))              :: b3(3) ! tcx: (4)

   logical :: precision_r4

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   ! allocation of variables

   allocate ( b1(5), source = (/ ( base(4)(x=1.0,y=2.0), i=1,5) /) ) ! tcx: (4)
   allocate ( b2(2,2), source = reshape ( source = (/ ( base(4)(x=3.0,y=4.0),i=1,4) /), shape = (/2,2/))) ! tcx: (4)
   b3       = (/ ( base(4)(x=5.0,y=6.0), i=1,3) /)  ! tcx: (4)

   ! I/O operations

   open ( u1%get(), file = 'array001k.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2

   inquire ( iolength = length1 )  b1           !<- output list is a single array item

   if ( length1 /= 40 )            error stop 101_4

   inquire ( iolength = length1 )  b2           !<- output list is a single array item

   if ( length1 /= 32 )            error stop 2_4

   inquire ( iolength = length1 )  b3           !<- output list is a single array item

   if ( length1 /= 24 )            error stop 3_4

   inquire ( iolength = length1 )  b1,b2,b3     !<- output list is a array item list

   if ( length1 /= 96 )            error stop 4_4

   inquire ( iolength = length1 )  b1(1:3:2),b2(2:1:-1,1),b3((/3,1/))     !<- output list is a array section list

   if ( length1 /= 48 )            error stop 5_4

   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b2, b3
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 6_4

   rewind u1%get()

   read ( u1%get(), iostat=stat1, iomsg=msg1 )       b3, b1, b2
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 7_4


   ! check if the values are read/written correctly

   if ( (.not. precision_r4(b1(1)%getX(),1.0)) .or. (.not. precision_r4(b1(1)%getY(),2.0)) .or. &
        (.not. precision_r4(b1(2)%getX(),1.0)) .or. (.not. precision_r4(b1(2)%getY(),2.0)) .or. &
        (.not. precision_r4(b1(3)%getX(),3.0)) .or. (.not. precision_r4(b1(3)%getY(),4.0)) .or. &
        (.not. precision_r4(b1(4)%getX(),3.0)) .or. (.not. precision_r4(b1(4)%getY(),4.0)) .or. &
        (.not. precision_r4(b1(5)%getX(),3.0)) .or. (.not. precision_r4(b1(5)%getY(),4.0))     )      error stop 8_4

   if ( (.not. precision_r4(b2(1,1)%getX(),3.0)) .or. (.not. precision_r4(b2(1,1)%getY(),4.0)) .or. &
        (.not. precision_r4(b2(2,1)%getX(),5.0)) .or. (.not. precision_r4(b2(2,1)%getY(),6.0)) .or. &
        (.not. precision_r4(b2(1,2)%getX(),5.0)) .or. (.not. precision_r4(b2(1,2)%getY(),6.0)) .or. &
        (.not. precision_r4(b2(2,2)%getX(),5.0)) .or. (.not. precision_r4(b2(2,2)%getY(),6.0))     )  error stop 9_4

   if ( (.not. precision_r4(b3(1)%getX(),1.0)) .or. (.not. precision_r4(b3(1)%getY(),2.0)) .or. &
        (.not. precision_r4(b3(2)%getX(),1.0)) .or. (.not. precision_r4(b3(2)%getY(),2.0)) .or. &
        (.not. precision_r4(b3(3)%getX(),1.0)) .or. (.not. precision_r4(b3(3)%getY(),2.0))     )      error stop 10_4

   ! close the file appropriately

   close ( u1%get(), status ='delete' )

end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: length1
   real(4) :: tmpr1, tmpr2

   select type (dtv)
      type is (base(4)) ! tcx: (4)
         inquire ( iolength = length1 ) dtv
   end select

   if ( length1 /= 8 ) error stop 11_4

   read ( u1%get(), iostat=iostat, iomsg=iomsg ) tmpr1, tmpr2

   call dtv%setX(tmpr1)
   call dtv%setY(tmpr2)

end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: length1

   select type (dtv)
      type is (base(4)) ! tcx: (4)
         inquire ( iolength = length1 ) dtv
   end select

   if ( length1 /= 8 ) error stop 12_4

   write ( u1%get(), iostat=iostat, iomsg=iomsg ) dtv%getX(), dtv%getY()

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 16 changes
