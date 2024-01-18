! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with private polymorphic component (output)
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

   type data (kd)
      integer, kind :: kd
      integer(kd), private :: i
   contains
      procedure, pass :: set => setdata
   end type

   type base (kb)
      integer, kind :: kb
      class(data(kb)), allocatable, private :: d
   contains
      procedure, pass :: set => setbase
      procedure, pass :: get => getbase
      procedure, pass :: check
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2
   type(base(4))               :: b3
   integer :: stat
   character(150) :: msg
   namelist /n123/ b1, b2, b3

   contains

   subroutine setbase(dtv, i)
      class(base(4)), intent(inout) :: dtv
      dtv%d%i = i
   end subroutine

   function getbase(dtv)
      class(base(4)), intent(in) :: dtv
      class(data(4)), allocatable :: getbase
      allocate (getbase, source = dtv%d)
   end function

   subroutine setdata(dtv,i)
      class(data(4)), intent(inout) :: dtv
      integer :: i
      dtv%i = i
   end subroutine

   subroutine start()
      allocate ( b1, source = base(4)( d=data(4)(101)))
      allocate ( b2, source = base(4)( d=data(4)(102)))
      allocate ( b3%d, source = data(4)(103) )
   end subroutine

   subroutine read(unit)
      integer, intent(in) :: unit
      read (unit, n123, iostat = stat, iomsg = msg )
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   end subroutine

   subroutine readdata(dtv,unit)

      interface read(formatted)
         subroutine readformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
            import data
            class(data(4)), intent(inout) :: dtv
            integer,  intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in)     :: v_list(:)
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
         end subroutine
      end interface

      class(data(4)), intent(inout) :: dtv
      integer, intent(in) :: unit
      namelist /dtio/ dtv
      read (unit, dtio, iostat=stat, iomsg = msg )
      if ( ( stat /= 0 ) .or. ( msg /= 'dataread' ) ) error stop 2_4

   end subroutine

   subroutine alloc ( dtv, tmp )
      class(base(4)), intent(inout) :: dtv
      class(data(4)), intent(in)    :: tmp
      if ( allocated(dtv%d) ) then
      	 deallocate(dtv%d)
      end if
      allocate( dtv%d , source = tmp )
   end subroutine

   logical function check(dtv, i)
      class(base(4)), intent(in) :: dtv
      integer(4), intent(in) :: i
      check = ( dtv%d%i == i )
   end function

end module

program access101akl
use m

   open (1, file = 'access101akl.1', form='formatted', access='sequential' )
   call start()
   call read (1)

   if ( .not. b1%check(1001) ) error stop 3_4
   if ( .not. b2%check(1002) ) error stop 4_4
   if ( .not. b3%check(1003) ) error stop 5_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, data, readdata, alloc

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   class(data(4)), allocatable :: tmp

   allocate(tmp)
   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   call readdata(tmp, unit)
   call alloc(dtv,tmp)

   iomsg = 'dtioread'

end subroutine

subroutine readformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: data

   class(data(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   integer(4) :: tmp
   if ( iotype /= "NAMELIST" )     error stop 8_4
   if ( size(v_list, 1) /= 0 )     error stop 9_4

   read (unit, *, iostat=iostat )   tmp

   call dtv%set(tmp)

   iomsg = 'dataread'

end subroutine
