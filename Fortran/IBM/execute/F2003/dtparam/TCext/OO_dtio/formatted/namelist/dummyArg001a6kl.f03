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

   integer :: unit = 1

contains

   subroutine writeBase(dtv,lb)
      class(base(*)), intent(in) :: dtv(3) ! tcx: (*)
      integer, intent(in) :: lb

      interface
         integer function externalWriteBase(dtv,lb)
            import base
            class(base(*)), intent(in) :: dtv(lb:(lb+2))  !<- contains 3 elements ! tcx: (*)
            integer, intent(in) :: lb
         end function
      end interface

      if ( ( externalWriteBase(dtv,lb) /=  0 ) ) error stop 1_4

   end subroutine

end module

program dummyArg001a6kl
   use m

   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:) ! tcx: (:)
   type(child(3,4))              :: b3(3) ! tcx: (3,4)
   type(child(:,4)), pointer     :: b4(:) ! tcx: (:,4)

   open (unit, file = 'dummyArg001a6kl.1', form='formatted', access='stream' )

   allocate(b1(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b2(3), source = (/ ( child(3,4)(c='IBM',i=j), j=1,3 ) /) ) ! tcx: (3,4)
   b3 = child(3,4)(c='jkl', i=4) ! tcx: (3,4)
   allocate(b4(3), source = (/ child(3,4)(c='mno',i=5), child(3,4)(c='pqr',i=6), child(3,4)(c='stu',i=7) /) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   call writeBase(b1,1)
   call writeBase(b2,2)
   call writeBase(b3,3)
   call writeBase(b4,4)

end program

integer function externalWriteBase(dtv,lb)
   use m, only: base, unit, write(formatted)
   class(base(*)), intent(in) :: dtv(lb:(lb+2))  !<- contains 3 elements ! tcx: (*)
   integer, intent(in) :: lb

   character(200) :: msg = ''

   namelist /nml/ dtv
   write ( unit, nml, iostat=externalWriteBase, iomsg = msg)

   if ( msg /= 'dtiowrite' ) error stop 2_4

end function

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( dtv )
      type is ( base(*) ) ! tcx: (*)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is ( child(*,4) ) ! tcx: (*,4)
         write (unit, "('i= ',I4,1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 11 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 8 changes