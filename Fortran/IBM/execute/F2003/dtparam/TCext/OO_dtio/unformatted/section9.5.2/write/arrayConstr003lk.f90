! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr003lk
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor with unlimited polymorphic variables
!*                               Sequential Access
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: i = -999
   end type

end module


program arrayConstr002
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg

   character(3) :: s1, s2, s3, s4, s5, s6, s7, s8, s10, s11, s12, s13, s14, s15, s16, s17
   integer :: i10, i11, i12, i13, i14, i15, i16, i17

   class(*), allocatable  :: u1,u2(:),u3(:,:)
   class(*), pointer      :: u4,u5(:),u6(:,:)

   ! allocation of variables

   allocate ( u1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( u3(2,2), source = reshape ( source = (/ base(3)('def'), base(3)('ghi'), base(3)('jkl'), base(3)('mno') /), shape = (/ 2, 2 /)) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( u5(3), source = (/ base(3)('pqr'), base(3)('stu'), base(3)('vwx')/) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   allocate ( u2(3), source = (/ child(3,4)('ABC',106), child(3,4)('DEF',107), child(3,4)('GHI',108)/) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( u4, source = child(3,4)('JKL',101) ) ! tcx: (3,4)
   allocate ( u6(2,2), source = reshape ( source = (/ child(3,4)('MNO',102), child(3,4)('PQR', 103), child(3,4)('STU',104), child(3,4)('VWX',105) /), shape = (/2,2 /)) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   open (unit = 1, file ='arrayConstr002.data', form='unformatted', access='sequential')

   ! I/O operations

   select type ( u1 )
      class is (base(*)) ! tcx: (*)
         select type ( u3 )
            class is (base(*)) ! tcx: (*)
               select type ( u5 )
                  class is (base(*)) ! tcx: (*)
                     write (1, iostat=stat, iomsg=msg ) (/ u1,u3,u5 /)
                     if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4
               end select
         end select
   end select

   select type ( u2 )
      class is (base(*)) ! tcx: (*)
         select type ( u4 )
            class is (base(*)) ! tcx: (*)
               select type ( u6 )
                  class is (base(*)) ! tcx: (*)
                     write (1, iostat=stat, iomsg=msg ) (/ u2(1:2) /) , (/ u2(3), u4, u6(1,1) /), (/  u6(2,1), u6(1:2,2) /)
                     if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
               end select
         end select
   end select

   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              s1, s2, s3, s4, s5, s6, s7, s8
   read (1, iostat=stat, iomsg=msg )              s10, i10, s11, i11, s12, i12, s13, i13, s14, i14, s15, i15, s16, i16, s17, i17

   ! check if the values are set correctly

   if ( ( s1 /= 'abc' ) .or. ( s5 /= 'mno' ) .or. &
        ( s2 /= 'def' ) .or. ( s6 /= 'pqr' ) .or. &
        ( s3 /= 'ghi' ) .or. ( s7 /= 'stu' ) .or. &
        ( s4 /= 'jkl' ) .or. ( s8 /= 'vwx' ) )    error stop 3_4

   if ( ( s10 /= 'ABC' ) .or. ( i10 /= 106 ) .or. &
        ( s11 /= 'DEF' ) .or. ( i11 /= 107 ) .or. &
        ( s12 /= 'GHI' ) .or. ( i12 /= 108 ) .or. &
        ( s13 /= 'JKL' ) .or. ( i13 /= 101 ) .or. &
        ( s14 /= 'MNO' ) .or. ( i14 /= 102 ) .or. &
        ( s15 /= 'PQR' ) .or. ( i15 /= 103 ) .or. &
        ( s16 /= 'STU' ) .or. ( i16 /= 104 ) .or. &
        ( s17 /= 'VWX' ) .or. ( i17 /= 105 ) )    error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%c, dtv%i
   end select

   iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 17 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 9 changes
