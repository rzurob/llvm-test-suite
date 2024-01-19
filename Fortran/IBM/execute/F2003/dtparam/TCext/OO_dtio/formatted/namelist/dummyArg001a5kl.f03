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
   class(base(:)), pointer :: b2(:,:) ! tcx: (:)

contains

   subroutine writeBase(dtv,lb1, lb2)
      class(base(*)), intent(in) :: dtv(5:,5:) ! tcx: (*)
      integer, intent(in) :: lb1, lb2

      integer :: stat
      character(200) :: msg

      if (( innerWriteBase(dtv,lb1, lb2) /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

      contains

         integer function innerWriteBase(dtv,lb1, lb2)
            class(base(*)), intent(in) :: dtv(lb1:, lb2:) ! tcx: (*)
            integer, intent(in) :: lb1, lb2

            namelist /nml/ dtv

            write ( unit, nml, iostat=innerWriteBase, iomsg = msg)

         end function

   end subroutine

end module

program dummyArg001a5kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(:)), allocatable :: b1(:,:) ! tcx: (:)
   type(child(3,4))              :: b3(2,2) ! tcx: (3,4)
   type(child(:,4)), pointer     :: b4(:,:) ! tcx: (:,4)

   open (unit, file = 'dummyArg001a5kl.1', form='formatted', access='stream' )

   allocate(b1(2,2), source = reshape ( source = (/ child(3,4)(c='abc',i=11), child(3,4)(c='def',i=12), child(3,4)(c='ghi',i=13), child(3,4)(c='jkl',i=14)  /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate(b2(2,2), source = reshape ( source = (/ child(3,4)(c='ABC',i=21), child(3,4)(c='DEF',i=22), child(3,4)(c='GHI',i=23), child(3,4)(c='JKL',i=24)  /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   b3 = reshape ( source =  (/ child(3,4)(c='mno',i=31), child(3,4)(c='pqr',i=32), child(3,4)(c='stu',i=33), child(3,4)(c='vwx',i=34)  /), shape = (/2,2/) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate(b4(2,2), source = reshape ( source = (/ child(3,4)(c='MNO',i=41), child(3,4)(c='PQR',i=42), child(3,4)(c='STU',i=43), child(3,4)(c='VWX',i=44)  /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   call writeBase(b1,10, 100)
   call writeBase(b2,11, 101)
   call writeBase(b3,12, 102)
   call writeBase(b4,13, 103)

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
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (kc) to invoke with (3,4) / declare with (*,4) - 19 changes
