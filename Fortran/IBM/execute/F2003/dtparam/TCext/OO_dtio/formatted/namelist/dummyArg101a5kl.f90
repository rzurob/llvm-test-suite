! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg101a5kl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object which is a assumed-shape array dummy argument
!*                                        which module procedure invokes inner function
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

   integer :: unit = 1
   class(base(:)), pointer :: b2(:,:) ! tcx: (:)

contains

   subroutine readBase(dtv,lb1,lb2)
      class(base(*)), intent(inout) :: dtv(5:,5:) ! tcx: (*)
      integer, intent(in) :: lb1, lb2

      integer :: stat
      character(200) :: msg

      if (( innerReadBase(dtv,lb1,lb2) /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

      contains

         integer function innerReadBase(dtv,lb1, lb2)
            class(base(*)), intent(inout) :: dtv(lb1:, lb2:) ! tcx: (*)
            integer, intent(in) :: lb1, lb2
            namelist /nml/ dtv
            read ( unit, nml, iostat=innerReadBase, iomsg = msg)

         end function

   end subroutine

end module

program dummyArg101a5kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1(:,:) ! tcx: (:)
   type(child(3,4))              :: b3(2,2) ! tcx: (3,4)
   type(child(:,4)), pointer     :: b4(:,:) ! tcx: (:,4)

   open (unit, file = 'dummyArg101a5kl.1', form='formatted', access='stream' )

   allocate( child(3,4) :: b1(2,2) ) ! tcx: (3,4)
   allocate( child(3,4) :: b2(2,2) ) ! tcx: (3,4)
   allocate( child(3,4) :: b4(2,2) ) ! tcx: child(3,4)

   call readBase(b1,10, 100)
   call readBase(b2,11, 101)
   call readBase(b3,12, 102)
   call readBase(b4,13, 103)

   select type ( b1 )
      type is ( base(*) ) ! tcx: (*)
         error stop 2_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b1(1,1)%c /= 'abc' ) .or. ( b1(1,1)%i /= 1 ) .or. ( b1(2,1)%c /= 'def' ) .or. ( b1(2,1)%i /= 2 ) .or. &
              ( b1(1,2)%c /= 'ghi' ) .or. ( b1(1,2)%i /= 3 ) .or. ( b1(2,2)%c /= 'jkl' ) .or. ( b1(2,2)%i /= 4 ))    error stop 3_4
   end select

   select type ( b2 )
      type is ( base(*) ) ! tcx: (*)
         error stop 4_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b2(1,1)%c /= 'ABC' ) .or. ( b2(1,1)%i /= 4 ) .or. ( b2(2,1)%c /= 'DEF' ) .or. ( b2(2,1)%i /= 5 ) .or. &
              ( b2(1,2)%c /= 'GHI' ) .or. ( b2(1,2)%i /= 6 ) .or. ( b2(2,2)%c /= 'JKL' ) .or. ( b2(2,2)%i /= 7 ) )    error stop 5_4
   end select

   if ( ( b3(1,1)%c /= 'jkl' ) .or. ( b3(1,1)%i /= 6 ) .or. ( b3(2,1)%c /= 'xxx' ) .or. ( b3(2,1)%i /= -999 ) .or. &
        ( b3(1,2)%c /= 'xxx' ) .or. ( b3(1,2)%i /= -999 ) .or. ( b3(2,2)%c /= 'mno' ) .or. ( b3(2,2)%i /= 7 ) )    error stop 6_4

   if ( ( b4(1,1)%c /= 'pqr' ) .or. ( b4(1,1)%i /= 8 ) .or. ( b4(2,1)%c /= 'stu' ) .or. ( b4(2,1)%i /= 9 ) .or. &
        ( b4(1,2)%c /= 'vwx' ) .or. ( b4(1,2)%i /= 10 ) .or. ( b4(2,2)%c /= 'xyz' ) .or. ( b4(2,2)%i /= 11 ) )     error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3,1X)", iostat=iostat )        dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, *, iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 7 changes
