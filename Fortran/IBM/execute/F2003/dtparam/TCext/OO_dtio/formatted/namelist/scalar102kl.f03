! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting inside DTIO procedure (input)
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
   type base (lb) ! lb=3
      integer, len :: lb
      character(lb), pointer :: c => null()
   end type

end module

module m1
   use m
   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface
end module


program scalar102kl
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(3))               :: b3 ! tcx: (3)
   type(base(:)), allocatable  :: b4 ! tcx: (:)
   type(base(:)), pointer      :: b5 ! tcx: (:)

   open (1, file = 'scalar102kl.1', form='formatted', access='sequential', status='old', BLANK='NULL' )

   allocate(base(3):: b1, b2, b4, b5) ! tcx: base(3)

   allocate(character(3):: b1%c, b2%c, b3%c, b4%c, b5%c) ! tcx: character(3)

   read  (1,*, iostat=stat, iomsg=msg)      b1
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
   read  (1,*, iostat=stat, iomsg=msg)      b2, b3
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4
   read  (1,*, iostat=stat, iomsg=msg)      b4
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
   read  (1,*, iostat=stat, iomsg=msg)      b5
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   if ( b1%c /= 'ABC' )  error stop 5_4
   if ( b2%c /= 'DEF' )  error stop 6_4
   if ( b3%c /= 'GHI' )  error stop 7_4
   if ( b4%c /= 'JKL' )  error stop 8_4
   if ( b5%c /= 'MNO' )  error stop 9_4

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m1, only: base

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   character(3), target :: c
   namelist /nml1/ c

   if ( iotype /= "LISTDIRECTED" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   read (unit, nml1, iostat=iostat )

   allocate( dtv%c, source = c )

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 7 changes
