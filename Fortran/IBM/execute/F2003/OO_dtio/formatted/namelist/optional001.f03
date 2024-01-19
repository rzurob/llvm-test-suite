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
      integer(4) :: i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
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

   subroutine write(unit, b1, b2)
      integer, intent(in) :: unit

      class(base), intent(in), optional :: b1
      class(base), intent(in), optional :: b2
      namelist /nml1/ b1
      namelist /nml2/ b2
      namelist /nml12/ b1, b2

      integer :: stat
      character(200) :: msg

      if       ( ( .not. present(b1) ) .and.  ( .not. present(b2) ) ) then
      	 write ( unit, null, iostat=stat, iomsg = msg)
      else if  ( ( present(b1) ) .and.  ( .not. present(b2) ) )       then
      	 write ( unit, nml1, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
      else if  ( ( .not. present(b1) ) .and.  ( present(b2) ) )       then
      	 write ( unit, nml2, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
      else
      	 write ( unit, nml12, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
      end if

   end subroutine

end module

program optional001
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2

   open (1, file = 'optional001.1', form='formatted', access='stream' )

   allocate( b1, source = base(1000))
   allocate( b2, source = base(1001))

   call write(1)
   call write(1, b1)
   call write(1, b2=b2)
   call write(1, b1, b2)
   call write1(1)
   call write1(1, b1)
   call write1(1, b2=b2)
   call write1(1, b1, b2)

contains

   subroutine write1(unit, b1, b2)
      integer, intent(in) :: unit

      class(base), intent(in), optional, allocatable :: b1
      class(base), intent(in), optional, pointer     :: b2

      namelist /nml1/ b1
      namelist /nml2/ b2
      namelist /nml12/ b1, b2

      integer :: stat
      character(200) :: msg

      if       ( ( .not. present(b1) ) .and.  ( .not. present(b2) ) ) then
      	 write ( unit, null, iostat=stat, iomsg = msg)
      else if  ( ( present(b1) ) .and.  ( .not. present(b2) ) )       then
      	 write ( unit, nml1, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
      else if  ( ( .not. present(b1) ) .and.  ( present(b2) ) )       then
      	 write ( unit, nml2, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4
      else
      	 write ( unit, nml12, iostat=stat, iomsg=msg )
      	 if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 6_4
      end if

   end subroutine

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   write (unit, "('i=',1X,I4)", iostat=iostat )              dtv%i

   iomsg = 'dtiowrite'

end subroutine

