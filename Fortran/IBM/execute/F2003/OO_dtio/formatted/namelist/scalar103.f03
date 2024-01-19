! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly scalar pointer/allocatable (Input)
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
   type, abstract :: base
      character(3) :: i
   end type

   type, extends(base) :: child
      character(3), allocatable :: i1
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

end module

program scalar103
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base) , pointer     :: b1
   class(base) , allocatable :: b2
   type(child)               :: b3
   type(child) , pointer     :: b4
   type(child) , allocatable :: b5

   namelist /nml/ b1, b2, b3, b4, b5

   open (1, file = 'scalar103.1', form='formatted', access='sequential' )
   allocate(child:: b1, b2)
   allocate(b4, b5)
   allocate(b3%i1, b4%i1, b5%i1)

   b1%i = 'xxx'
   b2%i = 'xxx'
   b3%i = 'xxx'
   b4%i = 'xxx'
   b5%i = 'xxx'

   read (1, nml, iostat=stat, iomsg=msg)
   if ( ( stat /=  0 ) .or. ( msg /= 'dtioread' ) )      error stop 1_4

   select type(b1)
      type is (child)
         if (( b1%i /= 'abc') .or. ( b1%i1 /= 'ABC' ) )  error stop 2_4
   end select

   select type(b2)
      type is (child)
         if ( ( b2%i /= 'def') .or. ( b2%i1 /= 'DEF' ) ) error stop 3_4
   end select

   if ( ( b3%i /= 'ghi' ) .or. (b3%i1 /= 'GHI') ) error stop 4_4
   if ( ( b4%i /= 'jkl' ) .or. (b4%i1 /= 'JKL') ) error stop 5_4
   if ( ( b5%i /= 'mno' ) .or. (b5%i1 /= 'MNO') ) error stop 6_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   select type ( i => dtv )
      type is (child)
         if ( .not. allocated(i%i1) ) then
            allocate (i%i1)
         end if
         read (unit, "(1X,A3,1X,A3,2X)", iostat=iostat )       i%i,i%i1
      class default
         error stop 9_4
   end select

   iomsg = 'dtioread'

end subroutine
