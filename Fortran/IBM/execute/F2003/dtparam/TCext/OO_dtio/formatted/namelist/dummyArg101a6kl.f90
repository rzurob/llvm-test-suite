! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg101a6kl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg101a6 by Robert Ma)
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
!*                                        Try namelist formatting for derived type object which is a explicit array dummy argument
!*                                        which module procedure, calls external procedure
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

contains

   subroutine readBase(dtv,lb)
      class(base(*)), intent(inout) :: dtv(3) ! tcx: (*)
      integer, intent(in) :: lb

      interface
         integer function externalReadBase(dtv,lb)
            import base
            class(base(*)), intent(inout) :: dtv(lb:(lb+2))  !<- contains 3 elements ! tcx: (*)
            integer, intent(in) :: lb
         end function
      end interface

      if ( ( externalReadBase(dtv,lb) /=  0 ) ) error stop 1_4

   end subroutine

end module

program dummyArg101a6kl
   use m

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:) ! tcx: (:)
   type(child(3,4))              :: b3(3) ! tcx: (3,4)
   type(child(:,4)), pointer     :: b4(:) ! tcx: (:,4)

   open (unit, file = 'dummyArg101a6kl.1', form='formatted', access='sequential' )

   allocate( base(3) :: b1(3) ) ! tcx: base(3)
   allocate( child(3,4) :: b2(3) ) ! tcx: (3,4)
   allocate( child(3,4) :: b4(3) ) ! tcx: child(3,4)

   call readBase(b1,1)
   call readBase(b2,2)
   call readBase(b3,3)
   call readBase(b4,4)

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) )    error stop 2_4

   select type ( b2 )
      type is ( base(*) ) ! tcx: (*)
         error stop 3_4
      type is ( child(*,4) ) ! tcx: (*,4)
         if ( ( b2(1)%c /= 'ABC' ) .or. ( b2(1)%i /= 4 ) .or. ( b2(2)%c /= 'DEF' ) .or. ( b2(2)%i /= 5 ) .or. &
              ( b2(3)%c /= 'xxx' ) .or. ( b2(3)%i /= -999 ) ) error stop 4_4
   end select

   if ( ( b3(1)%c /= 'jkl' ) .or. ( b3(1)%i /= 6 ) .or. ( b3(2)%c /= 'xxx' ) .or. ( b3(2)%i /= -999 ) .or. &
        ( b3(3)%c /= 'mno' ) .or. ( b3(3)%i /= 7 ) ) error stop 5_4

   if ( ( b4(1)%c /= 'vwx' ) .or. ( b4(1)%i /= 10 ) .or. ( b4(2)%c /= 'xxx' ) .or. ( b4(2)%i /= -999 ) .or. &
        ( b4(3)%c /= 'xyz' ) .or. ( b4(3)%i /= 11 ) )    error stop 6_4

end program

integer function externalreadBase(dtv,lb)
   use m, only: base, unit, read(formatted), readformatted
   class(base(*)), intent(inout) :: dtv(lb:(lb+2))  !<- contains 3 elements ! tcx: (*)
   integer, intent(in) :: lb

   character(200) :: msg = ''

   namelist /nml/ dtv
   read ( unit, nml, iostat=externalreadBase, iomsg = msg)
   if ( msg /= 'dtioread' ) error stop 7_4

end function

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         read (unit, *, iostat=iostat )        dtv%c
      type is ( child(*,4) ) ! tcx: (*,4)
         read (unit, *, iostat=iostat )  dtv%i
         if (iostat /= 0) return

         read (unit, "(A3)", iostat=iostat )  dtv%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 5 changes
