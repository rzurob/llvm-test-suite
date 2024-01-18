! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with optional attribute (output)
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
      integer(4) :: i = -7777
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

   integer :: i = -9999
   namelist /null/ i

contains

   subroutine read(unit, b1, b2)
      integer, intent(in) :: unit

      class(base), intent(inout), optional :: b1
      class(base), intent(inout), optional :: b2
      namelist /nml1/ b1
      namelist /nml2/ b2
      namelist /nml12/ b1, b2

      integer :: stat
      character(200) :: msg

      if       ( ( .not. present(b1) ) .and.  ( .not. present(b2) ) ) then
      	 read ( unit, null, iostat=stat, iomsg = msg)
      else if  ( ( present(b1) ) .and.  ( .not. present(b2) ) )       then
      	 read ( unit, nml1, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      else if  ( ( .not. present(b1) ) .and.  ( present(b2) ) )       then
      	 read ( unit, nml2, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
      else
      	 read ( unit, nml12, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
      end if

   end subroutine

end module

program optional101
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2

   open (1, file = 'optional101.1', form='formatted', access='stream' )

   allocate( b1, b2 )

   call read(1, b1)
   if ( b1%i /= 1002 ) error stop 4_4
   call read(1, b2=b2)
   if ( b2%i /= 1003 ) error stop 5_4
   call read(1, b1, b2)
   if ( ( b1%i /= 1004 ) .or. ( b2%i /= 1005 ) ) error stop 6_4

   call read1(1, b1)
   if ( b1%i /= 1006 ) error stop 7_4
   call read1(1, b2=b2)
   if ( b2%i /= 1007 ) error stop 8_4
   call read1(1, b1, b2)
   if ( ( b1%i /= 1008 ) .or. ( b2%i /= 1009 ) ) error stop 9_4

contains

   subroutine read1(unit, b1, b2)
      integer, intent(in) :: unit

      class(base), intent(inout), optional, allocatable :: b1
      class(base), intent(inout), optional, pointer     :: b2
      namelist /nml1/ b1
      namelist /nml2/ b2
      namelist /nml12/ b1, b2

      integer :: stat
      character(200) :: msg

      if       ( ( .not. present(b1) ) .and.  ( .not. present(b2) ) ) then
      	 read ( unit, null, iostat=stat, iomsg = msg)
      else if  ( ( present(b1) ) .and.  ( .not. present(b2) ) )       then
      	 read ( unit, nml1, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 10_4
      else if  ( ( .not. present(b1) ) .and.  ( present(b2) ) )       then
      	 read ( unit, nml2, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 11_4
      else
      	 read ( unit, nml12, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 12_4
      end if

   end subroutine
end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 13_4
   if ( size(v_list, 1) /= 0 ) error stop 14_4

   read (unit, "(I4)", iostat=iostat )              dtv%i

   iomsg = 'dtioread'

end subroutine

