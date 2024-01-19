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
!*                                        Try namelist formatting implicit scalar objects (Input)
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

program scalar109kl
   use m
   implicit type(base(4,3))  (A-M) ! tcx: (4,3)
   implicit class(base(4,3)) (N-Z) ! tcx: (4,3)
   allocatable :: z3
   pointer     :: z4

   namelist /nml1/ b1, b2
   namelist /nml2/ z3, z4

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'scalar109kl.1', form='formatted', access='stream' )

   allocate( z3, source = base(4,3)() ) ! tcx: (4,3)
   allocate( z4, source = base(4,3)() ) ! tcx: (4,3)

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   if ( ( b1%c /= 'abc' ) .or. ( b1%i /= 1234 ) )  error stop 3_4
   if ( ( b2%c /= 'def' ) .or. ( b2%i /= 2345 ) )  error stop 4_4
   if ( ( z3%c /= 'ghi' ) .or. ( z3%i /= 3456 ) )  error stop 5_4
   if ( ( z4%c /= 'jkl' ) .or. ( z4%i /= 4567 ) )  error stop 6_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   read (unit, "(I4,1X,A3)", iostat=iostat )        dtv%i, dtv%c

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 6 changes
