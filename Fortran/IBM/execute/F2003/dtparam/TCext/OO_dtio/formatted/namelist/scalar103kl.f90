! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar103kl
!*
!*  PROGRAMMER                 : David Forster (derived from scalar103 by Robert Ma)
!*  DATE                       : 2007-07-10 (original: 11/08/2004)
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
!*                                        Try namelist formatting with polymorphic/nonpoly scalar pointer/allocatable (Input)
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
   type, abstract :: base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: i
   end type

   type, extends(base) :: child (lc) ! lc=3
      integer, len :: lc
      character(lc), allocatable :: i1
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

end module

program scalar103kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)) , pointer     :: b1 ! tcx: (:)
   class(base(:)) , allocatable :: b2 ! tcx: (:)
   type(child(3,3))               :: b3 ! tcx: (3)
   type(child(3,3)) , pointer     :: b4 ! tcx: (:)
   type(child(3,3)) , allocatable :: b5 ! tcx: (:)

   namelist /nml/ b1, b2, b3, b4, b5

   open (1, file = 'scalar103kl.1', form='formatted', access='sequential' )
   allocate(child(3,3):: b1, b2) ! tcx: (3)
   allocate(child(3,3):: b4, b5) ! tcx: child(3,3)
   allocate(character(3):: b3%i1, b4%i1, b5%i1) ! tcx: character(3)

   b1%i = 'xxx'
   b2%i = 'xxx'
   b3%i = 'xxx'
   b4%i = 'xxx'
   b5%i = 'xxx'

   read (1, nml, iostat=stat, iomsg=msg)
   if ( ( stat /=  0 ) .or. ( msg /= 'dtioread' ) )      error stop 1_4

   select type(b1)
      type is (child(*,*)) ! tcx: (*)
         if (( b1%i /= 'abc') .or. ( b1%i1 /= 'ABC' ) )  error stop 2_4
   end select

   select type(b2)
      type is (child(*,*)) ! tcx: (*)
         if ( ( b2%i /= 'def') .or. ( b2%i1 /= 'DEF' ) ) error stop 3_4
   end select

   if ( ( b3%i /= 'ghi' ) .or. (b3%i1 /= 'GHI') ) error stop 4_4
   if ( ( b4%i /= 'jkl' ) .or. (b4%i1 /= 'JKL') ) error stop 5_4
   if ( ( b5%i /= 'mno' ) .or. (b5%i1 /= 'MNO') ) error stop 6_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   select type ( i => dtv )
      type is (child(*,*)) ! tcx: (*)
         if ( .not. allocated(i%i1) ) then
            allocate (character(3):: i%i1) ! tcx: character(3)
         end if
         read (unit, "(1X,A3,1X,A3,2X)", iostat=iostat )       i%i,i%i1
      class default
         error stop 9_4
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters (lc) to invoke with (3) / declare with (*) - 7 changes
