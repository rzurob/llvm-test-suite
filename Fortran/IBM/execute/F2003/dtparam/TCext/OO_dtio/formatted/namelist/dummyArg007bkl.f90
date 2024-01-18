! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg007bkl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg007b by Robert Ma)
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object with internal subroutine (Host Association)
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

   type :: base (lb)
      integer, len :: lb
      character(lb) ::  c
   end type

   type, extends(base) :: child
   end type

   type, extends(child) :: gen3 (kg)
      integer, kind :: kg
      integer(kg)   ::  i
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

   class(base(:)), pointer :: b1 ! tcx: (:)

end module

program dummyArg007bkl
   use m

   class(base(:)), allocatable :: b2 ! tcx: (:)

   namelist /nmlb1b2/  b1,b2

   integer :: stat
   character(200) :: msg = ''

   open (1, file = 'dummyArg007bkl.1', form='formatted', access='stream' )

   call writeB1B2(1)

   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   allocate(b1, source = gen3(3,4)(c='jkl',i=1) ) ! tcx: (3,4)

   call writeB1B2(1)

   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

contains

   subroutine writeB1B2(unit)
      integer, intent(in) :: unit

      if ( .not. associated (b1) ) then
         allocate(b1, source = child(3) (c='abc')     )
      end if
      if ( .not. allocated  (b2) ) then
         allocate(b2, source = gen3(3,4)(c='ghi',i=3) ) ! tcx: (3,4)
      end if

      write ( unit, nmlb1b2, iostat=stat, iomsg = msg )

   end subroutine

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, gen3

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type (dtv)
      type is (child(*))
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is (gen3(*,4)) ! tcx: (*,4)
         write (unit, "('i= ',I4, 1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
! type: child - parameters inherited from base - invoke with (3) / declare with (*) - 2 changes
! type: gen3 - added parameters (kg) to invoke with (3,4) / declare with (*,4) - 3 changes
