! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-08 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with scalar component with explicit shape array component (Output)
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

   type :: base (kb,lb1,lb2)
      integer, kind :: kb
      integer, len :: lb1,lb2
      type(mydata(kb)) :: b(lb1) ! tcx: (kb)
      character(lb2) :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
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

   class(base(4,:,:)), pointer :: b2 ! tcx: (4,:,:)
   namelist /nml1/ b2

end module

program scalar006kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:,:)), allocatable :: b1 ! tcx: (4,:,:)
   type(base(4,2,3))               :: b3 ! tcx: (4,2,3)
   class(base(4,:,:)), pointer     :: b4 ! tcx: (4,:,:)

   type(mydata(4)), allocatable :: d1 ! tcx: (4)
   type(mydata(4)):: d2 ! tcx: (4)
   type(mydata(4)), allocatable :: d3(:) ! tcx: (4)

   namelist /nml2/ b1
   namelist /nml3/ b3
   namelist /nml3/ b4

   open (1, file = 'scalar006kl.1', form='formatted', access='stream' )

   allocate (d1, source = mydata(4)(777) ) ! tcx: (4)
   d2 = mydata(4)(888) ! tcx: (4)
   allocate (d3(2), source = (/d1, d2/) )

   allocate(b1, source = base(4,2,3)(b=(/d2, d1/) , c='abc') ) ! tcx: (4,2,3)
   allocate(b2, source = base(4,2,3)(b=(/d1, d2/) , c='def') ) ! tcx: (4,2,3)
   b3 =  base(4,2,3) ( b = d3, c = 'ghi' ) ! tcx: (4,2,3)
   allocate(b4, source = base(4,2,3)(b=d3 , c='def') ) ! tcx: (4,2,3)

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

   class(base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(mydata(4)) ::  b(2) ! tcx: (4)

   namelist /nml/ b

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c

   b=dtv%b

   write (unit, nml, iostat=iostat, iomsg = iomsg )

   if ( ( iomsg /= 'dtiowrite1' ) .or. ( iostat /= 0 ) ) error stop 6_4

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

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i= ',I4,1X)", iostat=iostat )        dtv%i

   iomsg = 'dtiowrite1'

end subroutine


! Extensions to introduce derived type parameters:
! type: mydata - added parameters (kmd) to invoke with (4) / declare with (4) - 10 changes
! type: base - added parameters (kb,lb1,lb2) to invoke with (4) / declare with (2,3) - 10 changes
! type: base - added parameters (kb,lb1,lb2) to invoke with (4,2,3) / declare with (4,2,*) - 10 changes