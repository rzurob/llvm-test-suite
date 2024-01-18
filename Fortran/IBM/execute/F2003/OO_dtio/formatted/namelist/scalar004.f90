! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic pointer component with type (Output)
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

   type :: mydata
      integer(4) ::  i
   end type

   type :: base
      class(mydata), pointer  :: b => null()
      character(3) :: c
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

      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import mydata
         class(mydata), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), pointer :: b2
   namelist /nml1/ b2

end module

program scalar004
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   type(base)               :: b3
   class(base), pointer     :: b4

   class(mydata), allocatable, target :: d1

   namelist /nml2/ b1
   namelist /nml3/ b3
   namelist /nml3/ b4

   open (1, file = 'scalar004.1', form='formatted', access='stream' )

   allocate (d1, source = mydata(555) )

   allocate(b1, source = base(c='abc') )
   allocate(b2, source = base(c='def') )
   b3 =  base (c = 'ghi')
   allocate(b3%b, source = d1)
   allocate(b4, source = base(c='jkl'))
   allocate(b4%b, source = d1)

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, mydata

   interface write(formatted)
      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import mydata
         class(mydata), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   class(mydata), allocatable ::  b

   namelist /nml/ b

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c

   if ( associated(dtv%b) ) then
      allocate (b, source = dtv%b )
      write (unit, nml, iostat=iostat, iomsg = iomsg )
      if ( ( iomsg /= 'dtiowrite1' ) .or. ( iostat /= 0 ) ) error stop 6_4
   end if

   iomsg = 'dtiowrite'

end subroutine

subroutine writeformatteddata (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: mydata

   class(mydata), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   write (unit, "('i= ',I4,1X)", iostat=iostat )        dtv%i

   iomsg = 'dtiowrite1'

end subroutine
