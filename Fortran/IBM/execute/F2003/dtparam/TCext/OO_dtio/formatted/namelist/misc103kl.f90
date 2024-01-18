! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with sequence type (input)
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
   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      sequence
      integer(kb) :: i(lb)
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         type(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program misc103kl
   use m

   integer :: stat
   character(200) :: msg = ''
   type(base(4,2))               :: b1 ! tcx: (4,2)
   type(base(4,:)), pointer      :: b2 ! tcx: (4,:)
   type(base(4,:)), allocatable  :: b3 ! tcx: (4,:)

   namelist /nml/  b1, b2
   namelist /nml1/ b1, b2, b3

   open (1, file = 'misc103kl.1', form='formatted', access='sequential' )
   allocate(base(4,2):: b2, b3) ! tcx: base(4,3)

   b1%i = 0
   b2%i = 0
   b3%i = 0

   read (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )  error stop 1_4

   if ( ( b1%i(1)  /= 1 ) .or. ( b1%i(2)  /= 2 ) )  error stop 2_4
   if ( ( b2%i(1)  /= 3 ) .or. ( b2%i(2)  /= 4 ) )  error stop 3_4

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) )  error stop 4_4

   if ( ( b1%i(1)  /= 5 ) .or. ( b1%i(2)  /= 6 ) )  error stop 5_4
   if ( ( b2%i(1)  /= 7 ) .or. ( b2%i(2)  /= 8 ) )  error stop 6_4
   if ( ( b3%i(1)  /= 9 ) .or. ( b3%i(2)  /= 10 ) ) error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   type(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   namelist /dtio/ dtv

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, dtio, iostat=iostat )

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,2) - 5 changes
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 5 changes
