! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar008kl
!*
!*  DATE                       : 2007-07-08 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object,
!*                                        and invoke external procedure namelist formatting (Output)
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

   type :: base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      character(lb) ::  c
      integer(kb)   ::  i
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
   end interface

   class(base(4,:)), pointer :: b2 ! tcx: (4,:)
   namelist /nml1/ b2

end module

program scalar008kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   type(base(4,:)), pointer      :: b4 ! tcx: (4,:)

   namelist /nml2/ b1
   namelist /nml3/ b3
   namelist /nml3/ b4

   open (1, file = 'scalar008kl.1', form='formatted', access='stream' )

   allocate(b1, source = base(4,3)(c='abc',i=1) ) ! tcx: (4,3)
   allocate(b2, source = base(4,3)(c='def',i=2) ) ! tcx: (4,3)
   b3 =  base(4,3)(c='ghi',i=3) ! tcx: (4,3)
   allocate(b4, source = base(4,3)(c='jkl',i=4) ) ! tcx: (4,3)

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   procedure() :: writeChar

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "('i= ',I4,1X)", iostat=iostat )        dtv%i

   call writeChar(unit, dtv%c)

   iomsg = 'dtiowrite'

end subroutine

subroutine writeChar (unit,c)
   integer, intent(in) :: unit
   character(*), intent(in) :: c
   namelist /nmlc/ c

   write (unit,nmlc,iostat=iostat )

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 10 changes
