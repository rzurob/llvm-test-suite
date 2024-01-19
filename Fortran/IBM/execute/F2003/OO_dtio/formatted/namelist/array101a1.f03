! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array with abstract type with no components/zero-length
!*                                        character component(Input)
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
   end type

   type, extends(base) :: child
   end type

   type :: base1
      character(0) :: c = ''
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
      subroutine readformatted1(dtv, unit, iotype, v_list, iostat, iomsg )
         import base1
         class(base1), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   character(3) :: flag
   integer(4)   :: count

end module

program array101a1
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(child)              :: b2(5)
   class(base1), pointer    :: c1(:)
   type(base1)              :: c2(2,2,2)

   namelist /nml1/ c1
   namelist /nml2/ b1
   namelist /nml3/ b2
   namelist /nml4/ c2

   allocate( child :: b1(3) )
   allocate( c1(3) )

   open (1, file = 'array101a1.1', form='formatted', access='stream' )

   count = 0
   flag = ''

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) .or. ( count /= 3 ) .or. (flag /= 'ghi')) error stop 1_4

   count = 0
   flag = ''

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) .or. ( count /= 3 ) .or. (flag /= 'ghi')) error stop 2_4

   count = 0
   flag = ''

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) .or. ( count /= 5 ) .or. (flag /= 'mno')) error stop 3_4

   count = 0
   flag = ''

   read (1,NML=nml4, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) .or. ( count /= 8 ) .or. (flag /= 'vwx')) error stop 4_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, flag, count

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 5_4
   if ( size(v_list, 1) /= 0 ) error stop 6_4

   read ( unit, "(A3)", iostat = iostat ) flag
   count = count+1

   iomsg = 'dtioread'

end subroutine

subroutine readformatted1 (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base1, flag, count

   class(base1), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read ( unit, "(A3)", iostat = iostat ) flag
   count = count + 1

   iomsg = 'dtioread'

end subroutine
