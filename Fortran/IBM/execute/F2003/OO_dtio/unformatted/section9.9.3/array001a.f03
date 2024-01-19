! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - array non-polymorphic zero-sized type
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

end module

program array001a
   use m1

   ! declaration of variables
   type(base), allocatable :: b1(:)
   type(base), pointer     :: b2(:,:)
   type(base)              :: b3(3)

   logical :: precision_r4

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   ! allocation of variables

   allocate ( b1(5), source = (/ ( base(), i=1,5) /) )
   allocate ( b2(2,2), source = reshape ( source = (/ ( base(),i=1,4) /), shape = (/2,2/)))
   b3       = (/ ( base(), i=1,3) /)

   ! I/O operations

   open ( u1%get(), file = 'array001a.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2

   inquire ( iolength = length1 )  b1           !<- output list is a single array item

   if ( length1 /= 0 )            error stop 1_4

   inquire ( iolength = length1 )  b2           !<- output list is a single array item

   if ( length1 /= 0 )            error stop 2_4

   inquire ( iolength = length1 )  b3           !<- output list is a single array item

   if ( length1 /= 0 )            error stop 3_4

   inquire ( iolength = length1 )  b1,b2,b3     !<- output list is a array item list

   if ( length1 /= 0 )            error stop 4_4

   inquire ( iolength = length1 )  b1(1:3:2),b2(2:1:-1,1),b3((/3,1/))     !<- output list is a array section list

   if ( length1 /= 0 )            error stop 5_4

   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b2, b3
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 6_4

   rewind u1%get()

   read ( u1%get(), iostat=stat1, iomsg=msg1 )       b3, b1, b2
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 7_4

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

   select type (dtv)
      type is (base)
         inquire ( iolength = length1 ) dtv
   end select

   if ( length1 /= 0 ) error stop 11_4

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

   if ( length1 /= 0 ) error stop 12_4

end subroutine