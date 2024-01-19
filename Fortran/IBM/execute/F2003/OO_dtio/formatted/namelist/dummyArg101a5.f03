! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object which is a assumed-shape array dummy argument
!*                                        which module procedure invokes inner function
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

   type :: base
      character(3) ::  c = 'xxx'
   end type

   type, extends(base) :: child
      integer(4)   ::  i = -999
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: unit = 1
   class(base), pointer :: b2(:,:)

contains

   subroutine readBase(dtv,lb1,lb2)
      class(base), intent(inout) :: dtv(5:,5:)
      integer, intent(in) :: lb1, lb2

      integer :: stat
      character(200) :: msg

      if (( innerReadBase(dtv,lb1,lb2) /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

      contains

         integer function innerReadBase(dtv,lb1, lb2)
            class(base), intent(inout) :: dtv(lb1:, lb2:)
            integer, intent(in) :: lb1, lb2
            namelist /nml/ dtv
            read ( unit, nml, iostat=innerReadBase, iomsg = msg)

         end function

   end subroutine

end module

program dummyArg101a5
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:,:)
   type(child)              :: b3(2,2)
   type(child), pointer     :: b4(:,:)

   open (unit, file = 'dummyArg101a5.1', form='formatted', access='stream' )

   allocate( child :: b1(2,2) )
   allocate( child :: b2(2,2) )
   allocate( b4(2,2) )

   call readBase(b1,10, 100)
   call readBase(b2,11, 101)
   call readBase(b3,12, 102)
   call readBase(b4,13, 103)

   select type ( b1 )
      type is ( base )
         error stop 2_4
      type is ( child )
         if ( ( b1(1,1)%c /= 'abc' ) .or. ( b1(1,1)%i /= 1 ) .or. ( b1(2,1)%c /= 'def' ) .or. ( b1(2,1)%i /= 2 ) .or. &
              ( b1(1,2)%c /= 'ghi' ) .or. ( b1(1,2)%i /= 3 ) .or. ( b1(2,2)%c /= 'jkl' ) .or. ( b1(2,2)%i /= 4 ))    error stop 3_4
   end select

   select type ( b2 )
      type is ( base )
         error stop 4_4
      type is ( child )
         if ( ( b2(1,1)%c /= 'ABC' ) .or. ( b2(1,1)%i /= 4 ) .or. ( b2(2,1)%c /= 'DEF' ) .or. ( b2(2,1)%i /= 5 ) .or. &
              ( b2(1,2)%c /= 'GHI' ) .or. ( b2(1,2)%i /= 6 ) .or. ( b2(2,2)%c /= 'JKL' ) .or. ( b2(2,2)%i /= 7 ) )    error stop 5_4
   end select

   if ( ( b3(1,1)%c /= 'jkl' ) .or. ( b3(1,1)%i /= 6 ) .or. ( b3(2,1)%c /= 'xxx' ) .or. ( b3(2,1)%i /= -999 ) .or. &
        ( b3(1,2)%c /= 'xxx' ) .or. ( b3(1,2)%i /= -999 ) .or. ( b3(2,2)%c /= 'mno' ) .or. ( b3(2,2)%i /= 7 ) )    error stop 6_4

   if ( ( b4(1,1)%c /= 'pqr' ) .or. ( b4(1,1)%i /= 8 ) .or. ( b4(2,1)%c /= 'stu' ) .or. ( b4(2,1)%i /= 9 ) .or. &
        ( b4(1,2)%c /= 'vwx' ) .or. ( b4(1,2)%i /= 10 ) .or. ( b4(2,2)%c /= 'xyz' ) .or. ( b4(2,2)%i /= 11 ) )     error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (base)
         read (unit, "(A3,1X)", iostat=iostat )        dtv%c
      type is (child)
         read (unit, *, iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine
