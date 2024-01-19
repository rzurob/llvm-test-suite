! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-12 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object,
!*                                        and invoke external procedure namelist formatting (Input)
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

   type :: base (kb,lb) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb
      character(lb) ::  c = 'nil'
      integer(kb)   ::  i = -99
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scalar108kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   class(base(4,:)), pointer     :: b2 ! tcx: (4,:)
   type(base(4,3))               :: b3 ! tcx: (4,3)
   type(base(4,:)), pointer      :: b4 ! tcx: (4,:)

   namelist /nml1/ b1
   namelist /nml2/ b2
   namelist /nml3/ b3
   namelist /nml3/ b4

   open (1, file = 'scalar108kl.1', form='formatted', access='stream' )

   allocate(b1, source = base(4,3)() ) ! tcx: (4,3)
   allocate(b2, source = base(4,3)() ) ! tcx: (4,3)
   b3 =  base(4,3)() ! tcx: (4,3)
   allocate(b4, source = base(4,3)() ) ! tcx: (4,3)

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   if ( ( b1%i /= 1234 ) .or. ( b1%c /= 'abc' ) )  error stop 4_4
   if ( ( b2%i /= 2345 ) .or. ( b2%c /= 'def' ) )  error stop 5_4
   if ( ( b3%i /= 3456 ) .or. ( b3%c /= 'ghi' ) )  error stop 6_4
   if ( ( b4%i /= 4567 ) .or. ( b4%c /= 'jkl' ) )  error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   procedure() :: readChar

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, "(I4)", iostat=iostat )        dtv%i

   call readChar(unit, dtv%c)

   iomsg = 'dtioread'

end subroutine

subroutine readChar (unit,c)
   integer, intent(in) :: unit
   character(*), intent(inout) :: c
   namelist /nmlc/ c

   read (unit,nmlc,iostat=iostat )

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 10 changes
