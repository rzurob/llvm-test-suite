! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn102kl
!*
!*  DATE                       : 2007-07-05 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with non polymorphic array function return variable
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
   type base (lb)
      integer, len :: lb
      character(lb) :: c
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      integer(kc) :: i
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
   integer :: stat
   character(200) :: msg

end module

program funcRetrn102kl
   use m

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   type(child(:,4)),allocatable :: c1(:) ! tcx: (:,4)
   class(child(:,4)), pointer    :: c2(:) ! tcx: (:,4)

   open (1, file = 'funcRetrn102kl.1', form='formatted', access='sequential' )

   allocate ( b1(3), source = readbase()  )
   allocate ( c2(3), source = readchild() )

   if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) .or. ( b1(3)%c /= 'ghi' ) )  error stop 1_4
   if ( ( c2(1)%c /= 'ABC' ) .or. ( c2(2)%c /= 'DEF' ) .or. ( c2(3)%c /= 'GHI' ) .or. &
        ( c2(1)%i /= 101 ) .or. ( c2(2)%i /= 102 ) .or. ( c2(3)%i /= 103 ) )        error stop 2_4

   allocate ( child(3,4):: c1(3) ) ! tcx: child(3,4)
   c1 = readchild()

   if ( ( c1(1)%c /= 'JKL' ) .or. ( c1(2)%c /= 'MNO' ) .or. ( c1(3)%c /= 'PQR' ) .or. &
     ( c1(1)%i /= 201 ) .or. ( c1(2)%i /= 202 ) .or. ( c1(3)%i /= 203 ) )        error stop 3_4

contains

   type(base(3)) function readBase() ! tcx: (3)
      dimension :: readBase(3)
      namelist /WB/ readBase
      read (unit, WB, iostat = stat, iomsg = msg )
   end function

   function readChild()
      type(child(3,4)) :: readChild(3) ! tcx: (3,4)
      namelist /WC/ readChild
      read (unit, WC, iostat = stat, iomsg = msg )
   end function

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   select type (dtv)
      type is (base(*)) ! tcx: (*)
         read (unit, "(A3)", iostat=iostat )            dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, "(I3,1X,A3)", iostat=iostat )      dtv%i, dtv%c
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 5 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 4 changes
