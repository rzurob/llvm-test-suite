! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor with unlimited polymorphic mixed
!*                                 with polymorphic and non-polymorphic variables
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

   character(3) :: s1, s2, s3, s4, s5, s6
   character(3) :: s11, s12, s13, s14, s15, s16
   character(3) :: s21, s22, s23, s24, s25, s26
   character(3) :: s31, s32, s33, s34, s35, s36

   integer :: i1, i2, i3, i4, i5, i6
   integer :: i11, i12, i13, i14, i15, i16
   integer :: i21, i22, i23, i24, i25, i26

   class(*), allocatable  :: u1,u2(:)
   class(base(:)), pointer   :: b1(:) ! tcx: (:)
   class(child(:,4)), pointer  :: c1(:) ! tcx: (:,4)

   type(base(3)) :: b2(2) = (/ base(3)('abc'), base(3)('def') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(child(3,4)):: c2(2) = (/ child(3,4)('aaa', 1001), child(3,4)('bbb', 1002) /) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   ! allocation of variables

   allocate ( u1, source = child(3,4)('abc',101) ) ! tcx: (3,4)
   allocate ( u2(3), source = (/ child(3,4)('ABC',102), child(3,4)('DEF',103), child(3,4)('GHI',104)/) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( b1(2), source = (/ child(3,4)('JKL',105), child(3,4)('MNO',106) /) ) ! tcx: (3,4) ! tcx: (3,4)
   allocate ( c1(2), source = (/ child(3,4)('PQR',107), child(3,4)('STU',108) /) ) ! tcx: (3,4) ! tcx: (3,4)

   open (unit = 1, file ='arrayConstr002.data', form='unformatted', access='sequential')

   ! I/O operations

   select type ( u1 )
      class is (base(*)) ! tcx: (*)
         select type ( u2 )
            class is (base(*)) ! tcx: (*)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,b1 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 101_4
         end select
   end select

   select type ( u1 )
      class is (child(*,4)) ! tcx: (*,4)
         select type ( u2 )
            class is (child(*,4)) ! tcx: (*,4)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,c1 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
         end select
   end select

   select type ( u1 )
      type is (child(*,4)) ! tcx: (*,4)
         select type ( u2 )
            type is (child(*,4)) ! tcx: (*,4)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,c2 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
         end select
   end select

   deallocate( u1, u2 )
   allocate ( u1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( u2(3), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI')/) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   select type ( u1 )
      type is (base(*)) ! tcx: (*)
         select type ( u2 )
            type is (base(*)) ! tcx: (*)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,b2 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
         end select
   end select

   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              s1, i1, s2, i2, s3, i3, s4, i4, s5, i5, s6, i6
   read (1, iostat=stat, iomsg=msg )              s11, i11, s12, i12, s13, i13, s14, i14, s15, i15, s16, i16
   read (1, iostat=stat, iomsg=msg )              s21, i21, s22, i22, s23, i23, s24, i24, s25, i25, s26, i26
   read (1, iostat=stat, iomsg=msg )              s31, s32, s33, s34, s35, s36

   ! check if the values are set correctly

   if ( ( s1 /= 'abc' ) .or. ( i1 /= 101 ) .or. &
        ( s2 /= 'ABC' ) .or. ( i2 /= 102 ) .or. &
        ( s3 /= 'DEF' ) .or. ( i3 /= 103 ) .or. &
        ( s4 /= 'GHI' ) .or. ( i4 /= 104 ) .or. &
        ( s5 /= 'JKL' ) .or. ( i5 /= 105 ) .or. &
        ( s6 /= 'MNO' ) .or. ( i6 /= 106 ) )    error stop 6_4

   if ( ( s11 /= 'abc' ) .or. ( i11 /= 101 ) .or. &
        ( s12 /= 'ABC' ) .or. ( i12 /= 102 ) .or. &
        ( s13 /= 'DEF' ) .or. ( i13 /= 103 ) .or. &
        ( s14 /= 'GHI' ) .or. ( i14 /= 104 ) .or. &
        ( s15 /= 'PQR' ) .or. ( i15 /= 107 ) .or. &
        ( s16 /= 'STU' ) .or. ( i16 /= 108 ) )  error stop 7_4

   if ( ( s21 /= 'abc' ) .or. ( i21 /= 101 ) .or. &
        ( s22 /= 'ABC' ) .or. ( i22 /= 102 ) .or. &
        ( s23 /= 'DEF' ) .or. ( i23 /= 103 ) .or. &
        ( s24 /= 'GHI' ) .or. ( i24 /= 104 ) .or. &
        ( s25 /= 'aaa' ) .or. ( i25 /= 1001 ) .or. &
        ( s26 /= 'bbb' ) .or. ( i26 /= 1002 ) )  error stop 8_4

   if ( ( s31 /= 'abc' ) .or. &
        ( s32 /= 'ABC' ) .or. &
        ( s33 /= 'DEF' ) .or. &
        ( s34 /= 'GHI' ) .or. &
        ( s35 /= 'abc' ) .or. &
        ( s36 /= 'def' )  )  error stop 9_4

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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 15 changes
! type: child - added parameters (kchild_1) to invoke with (3,4) / declare with (*,4) - 17 changes
