! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object external subroutine with internal sub(Host Association)
!*  KEYWORD(S)                 :          With different class dummy arg
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type :: base (lb)
      integer, len :: lb
      character(lb) ::  c
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   ::  i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(child(:,4)), pointer :: b2 ! tcx: (:,4)

end module

program dummyArg007a1kl
   use m

   interface
      subroutine writeBase(dtv, unit)
         import child
         class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
         integer, intent(in)     :: unit
      end subroutine
   end interface

   integer :: stat
   character(200) :: msg = ''
   class(child(:,4)), allocatable :: b1 ! tcx: (:,4)

   open (1, file = 'dummyArg007a1kl.1', form='formatted', access='stream' )

   allocate(b1, source = child(3,4)(c='abc',i=1) ) ! tcx: (3,4)
   allocate(b2, source = child(3,4)(c='ghi',i=3) ) ! tcx: (3,4)

   call writeBase(b1,1)
   call writeBase(b2,1)

end program

subroutine writeBase(dtv, unit)
   use m
   class(child(*,4)), intent(in) :: dtv ! tcx: (*,4)
   integer, intent(in) :: unit
   integer :: stat
   character(200) :: msg

   namelist /nml/ dtv
   call internalWriteBase(unit, dtv)

contains

   subroutine internalWriteBase(unit, dtv)
      class(base(*)), intent(in) :: dtv ! tcx: (*)
      integer, intent(in) :: unit
      write ( unit, nml, iostat=stat, iomsg = msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
   end subroutine

end subroutine

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, "('i= ',I4, 1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 7 changes