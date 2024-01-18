! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg006akl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg006a by Robert Ma)
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
!*                                        Try namelist formatting for derived type object with module subroutine (Host Association)
!*                                        Try internal file
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

   class(base(:)), pointer :: b1 ! tcx: (:)
   class(base(:)), allocatable :: b2 ! tcx: (:)
   character(30) :: internalFile (10)
   namelist /nmlb1b2/ b1, b2

contains

   subroutine writeB1B2(unit)
      class(*), intent(inout) :: unit(:)

      integer :: stat
      character(200) :: msg
      select type(unit)
         type is (character(*))  !<- internal file
            write ( unit, nmlb1b2, iostat=stat, iomsg = msg)
            if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
      end select

   end subroutine

end module

program dummyArg006akl
   use m

   integer :: stat
   character(200) :: msg = ''

   allocate(b1, source = base(3) (c='abc')     ) ! tcx: (3)
   allocate(b2, source = child(3,4)(c='ghi',i=3) ) ! tcx: (3,4)

   call writeB1B2(internalFile)
   call writeB1B2(internalFile(4:6))

   print *, internalFile(1)
   print *, internalFile(2)
   print *, internalFile(3)
   print *, internalFile(4)
   print *, internalFile(5)
   print *, internalFile(6)

end program


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
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 2 changes
