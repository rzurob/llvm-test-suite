! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-02 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - STREAM= specifier: Try using INQUIRE stmt with STREAM= specifier in procedures
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
   type, abstract :: base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      real(kbase_1) :: x
   contains
      procedure, nopass :: gettype => gettypebase
      procedure, pass   :: getX
      procedure(inf), pass, deferred :: getY
      procedure, pass   :: setX
      procedure(inf2), pass, deferred :: setY
   end type

   type, extends(base) :: child
      real(kbase_1) :: y
   contains
      procedure, nopass :: gettype => gettypechild
      procedure, pass   :: getY
      procedure, pass   :: setY
   end type

   type :: unit (kunit_1) ! kunit_1=4
      integer, kind :: kunit_1
      integer(kunit_1) :: unumber
   contains
      procedure, pass :: get
      procedure, pass :: isStream
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

   interface
      real function inf(a)
         import base
         class(base(4)), intent(in) :: a ! tcx: (4)
      end function
   end interface

   interface
      subroutine inf2(a, i)
         import base
         class(base(4)), intent(inout) :: a ! tcx: (4)
         real(4) :: i
      end subroutine
   end interface

   type(unit(4)) :: u1 = unit(4)(2) ! tcx: (4) ! tcx: (4)

contains

   integer(4) function gettypebase()
      gettypebase = 1
   end function

   integer(4) function gettypechild()
      gettypechild = 2
   end function

   integer(4) function get(a)
      class(unit(4)), intent(in) :: a ! tcx: (4)
      get = a%unumber
   end function

   logical function isStream(a)
      class(unit(4)), intent(in) :: a ! tcx: (4)
      character(1) :: tmp
      inquire ( a%get(), stream=tmp )

      if ( tmp .eq. 'Y' ) then
         isStream = .true.
      else
         isStream = .false.
      end if
   end function

   real(4) function getX(a)
      class(base(4)), intent(in) :: a ! tcx: (4)
      getX = a%x
   end function

   real(4) function getY(a)
      class(child(4)), intent(in) :: a ! tcx: (4)
      getY = a%y
   end function

   subroutine setX(a, i)
      class(base(4)), intent(inout) :: a ! tcx: (4)
      real(4) :: i
      a%x = i
   end subroutine

   subroutine setY(a, i)
      class(child(4)), intent(inout) :: a ! tcx: (4)
      real(4) :: i
      a%y = i
   end subroutine

end module


program stream001ak
   use m1

   ! declaration of variables
   class(base(4)), allocatable :: b1(:) ! tcx: (4)
   class(base(4)), pointer     :: b2(:) ! tcx: (4)
   logical :: precision_r4

   character(200) :: msg1 = ''
   integer :: stat1

   ! allocation of variables

   allocate ( b1(2), source = (/ child(4)(x=1.0,y=2.0), child(4)(x=1.0,y=2.0) /) ) ! tcx: (4) ! tcx: (4)
   allocate ( b2(0), source = (/ (child(4)(1.0,2.0), i=1, 0) /) ) ! tcx: (4)

   ! I/O operations

   if ( u1%isStream() )      error stop 101_4   !<- initially, unit is not defined with stream access, stream= specifier shall set variable to UNKNOWN

   open ( u1%get(), file = 'stream001ak.data', form='unformatted', access='direct', recl=16 )   !<- open a external file with unit #2

   if ( u1%isStream() )      error stop 2_4

   write ( u1%get(), iostat=stat1, iomsg=msg1, rec=11 ) b1
   if ( stat1 /= 0 ) error stop 3_4
   write ( u1%get(), iostat=stat1, iomsg=msg1, rec=12 ) b2
   if ( stat1 /= 0 ) error stop 4_4

   deallocate (b2)
   allocate( b2(2), source = (/ (child(4)(0.0,0.0), i=1, 2) /)  ) ! tcx: (4)

   read  ( u1%get(), iostat=stat1, iomsg=msg1, rec=11 ) b2
   if ( stat1 /= 0 ) error stop 5_4

   read  ( u1%get(), iostat=stat1, iomsg=msg1, rec=11) b1
   if ( stat1 /= 0 ) error stop 6_4

   ! check if the values are read/write correctly

   if ( (.not. precision_r4(b1(1)%getx(),1.0) ) .or. (.not. precision_r4(b1(1)%gety(),2.0) ) ) error stop 7_4
   if ( (.not. precision_r4(b1(2)%getx(),1.0) ) .or. (.not. precision_r4(b1(2)%gety(),2.0) ) ) error stop 8_4
   if ( (.not. precision_r4(b2(1)%getx(),1.0) ) .or. (.not. precision_r4(b2(1)%gety(),2.0) ) ) error stop 9_4
   if ( (.not. precision_r4(b2(2)%getx(),1.0) ) .or. (.not. precision_r4(b2(2)%gety(),2.0) ) ) error stop 10_4

   ! close the file appropriately

   close ( u1%get(), status ='delete' )

end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4) :: tmp1 = 0
   integer(4) :: tmp2 = 0

   real(4) :: tmpr1, tmpr2

   if ( u1%isStream() )   error stop 11_4

   read ( u1%get(), iostat=tmp1, iomsg=iomsg ) tmpr1

   call dtv%setX(tmpr1)

   associate( mydtv => dtv )
      if ( mydtv%gettype() == 2 ) then !<- dynamic type is child
         read ( u1%get(), iostat=tmp2, iomsg=iomsg ) tmpr2
         call dtv%setY(tmpr2)
      end if
   end associate

   iostat = tmp1 + tmp2

end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4) :: tmp1 = 0
   integer(4) :: tmp2 = 0

   if ( u1%isStream() )   error stop 12_4

   write ( u1%get(), iostat=tmp1, iomsg=iomsg ) dtv%getX()

   associate( mydtv => dtv )
      if ( mydtv%gettype() == 2 ) then !<- dynamic type is child
         write ( u1%get(), iostat=tmp2, iomsg=iomsg ) dtv%getY()
      end if
   end associate

   iostat = tmp1 + tmp2

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 10 changes
! type: child - added parameters () to invoke with (4) / declare with (4) - 6 changes
! type: unit - added parameters (kunit_1) to invoke with (4) / declare with (4) - 8 changes
