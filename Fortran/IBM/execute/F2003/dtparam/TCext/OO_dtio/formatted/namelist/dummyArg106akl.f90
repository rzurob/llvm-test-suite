! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg106akl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg106a by Robert Ma)
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
!*                                        Try internal file and input statement
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
      character(lb) ::  c = 'xxx'
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc)   ::  i = -999
   end type

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

   class(base(:)), pointer :: b1 ! tcx: (:)
   class(base(:)), allocatable :: b2 ! tcx: (:)
   character(29) :: internalFile (10)
   namelist /nmlb1b2/ b1, b2

contains

   subroutine readB1B2(unit)
      class(*), intent(in) :: unit(:)

      integer :: stat
      character(200) :: msg
      select type(unit)
         type is (character(*))  !<- internal file
            read ( unit, nmlb1b2, iostat=stat, iomsg = msg)
            if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      end select

   end subroutine

end module

program dummyArg106akl
   use m

   integer :: stat
   character(200) :: msg = ''

   allocate(b1, source = base(3) () ) ! tcx: (3)
   allocate(b2, source = child(3,4)() ) ! tcx: (3,4)

   write ( internalFile(1), * ) " &NMLB1B2"
   write ( internalFile(2), * ) " B1= abc"
   write ( internalFile(3), * ) " B2= 1234 def"
   write ( internalFile(4), * ) " B1= ABC"
   write ( internalFile(5), * ) " B2= 4321 DEF   /"
   write ( internalFile(8), * ) " &NMLB1B2"
   write ( internalFile(9), * ) " B1= 9876 abc"
   write ( internalFile(10), * )" B2= 1234 def    /"

   call readB1B2(internalFile)

   if ( b1%c /= 'ABC' ) error stop 2_4
   select type (b2)
      type is (child(*,4)) ! tcx: (*,4)
         if (( b2%c /= 'DEF' ) .or. (b2%i /= 4321)) error stop 3_4
      class default
         error stop 4_4
   end select

   allocate (b1, source=child(3,4)() ) ! tcx: (3,4)
   call readB1B2(internalFile(8:10))
   select type (b1)
      type is (child(*,4)) ! tcx: (*,4)
         if (( b1%c /= 'abc' ) .or. (b1%i /= 9876)) error stop 5_4
      class default
         error stop 6_4
   end select

   select type (b2)
      type is (child(*,4)) ! tcx: (*,4)
         if (( b2%c /= 'def' ) .or. (b2%i /= 1234)) error stop 7_4
      class default
         error stop 8_4
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat )        dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, "(I4,1X,A3)", iostat=iostat )  dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 6 changes
