! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
   type :: base (kbase_1,kbase_2,lbase_1) ! kbase_1,kbase_2,lbase_1=4,8,3
      integer, kind :: kbase_1,kbase_2
      integer, len :: lbase_1
      integer(kbase_2)   :: x
      real(kbase_1)      :: y
      character(lbase_1) :: z
   contains
      procedure, pass   :: getX
      procedure, pass   :: getY
      procedure, pass   :: getZ
      procedure, pass   :: setX
      procedure, pass   :: setY
      procedure, pass   :: setZ
   end type

   type, extends(base) :: child (kchild_1,kchild_2) ! kchild_1,kchild_2=4,2
      integer, kind :: kchild_1,kchild_2
      integer(kchild_2) :: a
      real(kchild_1)    :: b
      complex(kchild_1) :: c
   end type

   type :: unit
      integer(4) :: unumber
   contains
      procedure, pass :: get
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(4,8,*)), intent(inout) :: dtv ! tcx: (4,8,*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(4,8,*)), intent(in) :: dtv ! tcx: (4,8,*)
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
      class(base(4,8,*)), intent(in) :: a ! tcx: (4,8,*)
      getX = a%x
   end function

   real(4) function getY(a)
      class(base(4,8,*)), intent(in) :: a ! tcx: (4,8,*)
      getY = a%y
   end function

   character(3) function getZ(a)
      class(base(4,8,*)), intent(in) :: a ! tcx: (4,8,*)
      getZ = a%z
   end function

   subroutine setX(a, i)
      class(base(4,8,*)), intent(inout) :: a ! tcx: (4,8,*)
      integer(8) :: i
      a%x = i
   end subroutine

   subroutine setY(a, i)
      class(base(4,8,*)), intent(inout) :: a ! tcx: (4,8,*)
      real(4) :: i
      a%y = i
   end subroutine

   subroutine setZ(a, i)
      class(base(4,8,*)), intent(inout) :: a ! tcx: (4,8,*)
      character(3) :: i
      a%z = i
   end subroutine

end module

program array002klk
   use m1

   ! declaration of variables
   class(base(4,8,:)), allocatable :: b1(:) ! tcx: (4,8,:)
   class(base(4,8,:)), pointer     :: b2(:,:) ! tcx: (4,8,:)
   class(base(4,8,:)), allocatable :: b3(:) ! tcx: (4,8,:)
   class(base(4,8,:)), pointer     :: b4(:,:) ! tcx: (4,8,:)

   logical :: precision_r4

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   ! allocation of variables

   allocate ( b1(3), source = (/ (base(4,8,3)(x=1,y=2.0,z='ibm'), i=1,3 ) /) ) ! tcx: (4,8,3)
   allocate ( b2(2,2), source = reshape ( source=(/(child(4,8,3,4,2)(x=3,y=4.0,z='ftn', a=5, b=6.0, c=(7.0,8.0)),i=1,4)/), shape= (/2,2/) )) ! tcx: (4,8,3,4,2)
   allocate (base(4,8,3):: b3(3) ) ! tcx: base(4,8,3)
   allocate ( child(4,8,3,4,2) :: b4(2,2) ) ! tcx: (4,8,3,4,2)

   ! I/O operations

   open ( u1%get(), file = 'array002klk.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2

   select type (b1)
      type is (base(4,8,*))  ! tcx: (4,8,*)
         inquire ( iolength = length1 )   b1           !<- output list is a single scalar item
      class default
         error stop 101_4
   end select

   if ( length1 /= 48 )             error stop 2_4

   select type (b2)
      type is (base(4,8,*))  ! tcx: (4,8,*)
         error stop 3_4
      type is (child(4,8,*,4,2)) ! tcx: (4,8,*,4,2)
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
      type is (child(4,8,*,4,2)) ! tcx: (4,8,*,4,2)
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
   class(base(4,8,*)), intent(inout) :: dtv ! tcx: (4,8,*)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer :: length1

   select type ( a => dtv )
      type is (base(4,8,*)) ! tcx: (4,8,*)
         inquire ( iolength = length1 ) a
         if ( length1 /= 16 )    error stop 10_4
         read ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z
      type is (child(4,8,*,4,2)) ! tcx: (4,8,*,4,2)
         inquire ( iolength = length1 ) a
         if ( length1 /= 32 )    error stop 11_4
         read ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z, a%a, a%b, a%c
      class default
         error stop 12_4
   end select

end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base(4,8,*)), intent(in) :: dtv ! tcx: (4,8,*)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( a => dtv )
      type is (base(4,8,*)) ! tcx: (4,8,*)
         inquire ( iolength = length1 ) a
         if ( length1 /= 16 )    error stop 13_4
                               write ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z
      type is (child(4,8,*,4,2)) ! tcx: (4,8,*,4,2)
         inquire ( iolength = length1 ) a
         if ( length1 /= 32 )    error stop 14_4
         write ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z, a%a, a%b, a%c
      class default
         error stop 15_4
   end select

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2,lbase_1) to invoke with (4,8,3) / declare with (4,8,*) - 19 changes
! type: child - added parameters (kchild_1,kchild_2) to invoke with (4,8,3,4,2) / declare with (4,8,*,4,2) - 6 changes