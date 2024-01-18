! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-07 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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

   type :: mydata (kmd)
      integer, kind :: kmd
      integer(kmd) ::  i
   end type

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      class(mydata(kb)), pointer  :: b => null() ! tcx: (kb)
      character(lb) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine writeformatteddata(dtv, unit, iotype, v_list, iostat, iomsg )
         import mydata
         class(mydata(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,:)), pointer :: b2 ! tcx: (4,:)
   namelist /nml1/ b2

end module

program scalar004kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   class(base(4,:)), pointer     :: b4 ! tcx: (4,:)

   class(mydata(4)), allocatable, target :: d1 ! tcx: (4)

   namelist /nml2/ b1
   namelist /nml3/ b3
   namelist /nml3/ b4

   open (1, file = 'scalar004kl.1', form='formatted', access='stream' )

   allocate (d1, source = mydata(4)(555) ) ! tcx: (4)

   allocate(b1, source = base(4,3)(c='abc') ) ! tcx: (4,3)
   allocate(b2, source = base(4,3)(c='def') ) ! tcx: (4,3)
   b3 =  base(4,3) (c = 'ghi') ! tcx: (4,3)
   allocate(b3%b, source = d1)
   allocate(b4, source = base(4,3)(c='jkl')) ! tcx: (4,3)
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
         class(mydata(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   class(mydata(4)), allocatable ::  b ! tcx: (4)

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

   class(mydata(4)), intent(in) :: dtv ! tcx: (4)
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


! Extensions to introduce derived type parameters:
! type: mydata - added parameters (kmd) to invoke with (4) / declare with (4) - 7 changes
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 10 changes
