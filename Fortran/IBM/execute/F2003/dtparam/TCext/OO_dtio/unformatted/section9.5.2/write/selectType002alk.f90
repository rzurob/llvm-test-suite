! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write selector (array/array section) in class default in select type construct
!*                               Sequential Access
!*
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type

   type, extends(base) :: child
      character(lbase_1) :: cc = ''
   end type

end module


module m1
   use m

   type, extends(child) :: gen3 (kgen3_1) ! kgen3_1=4
      integer, kind :: kgen3_1
      integer(kgen3_1) :: i = -999
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program selectType002alk
   use m1

   ! declaration of variables
   class(base(:)), pointer     :: b1(:) ! tcx: (:)
   class(base(:)), allocatable :: b2(:), b3(:,:) ! tcx: (:)

   class(child(:)), pointer    :: c1(:) ! tcx: (:)
   class(child(:)), allocatable:: c2(:,:) ! tcx: (:)

   class(gen3(:,4)), pointer     :: g1(:) ! tcx: (:,4)

   integer :: stat
   character(200) :: msg =''

   character(6) :: cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9, cc10, cc11, cc12, cc13, cc14, cc15
   integer(4)   :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10

   ! allocation of variables

   allocate ( b1(2)   , source = (/ base(3)('abc'), base(3)('def') /) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2(3)   , source = (/ child(3)('abc','def'), child(3)(), child(3)('ghi', 'jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b3(2,2) , source = reshape( source = (/ gen3(3,4) ('ABC','DEF', 101 ),   & ! tcx: (3,4)
   gen3(3,4) ('GHI','JKL', 102 ), gen3(3,4) ('MNO','PQR', 103 ), gen3(3,4) ('STU','VWX', 104 ) /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   allocate ( c1(2), source = (/ child(3)('ABC','DEF'), child(3)('GHI', 'JKL') /) ) ! tcx: (3) ! tcx: (3)
   allocate ( c2(2,2), source = reshape( source = (/ gen3(3,4) ('abc','def', 201 ),   & ! tcx: (3,4)
   gen3(3,4) ('ghi','jkl', 202 ), gen3(3,4) ('mno','pqr', 203 ), gen3(3,4) ('stu','vwx', 204 ) /), shape = (/2,2/) ) ) ! tcx: (3,4) ! tcx: (3,4) ! tcx: (3,4)

   allocate ( g1(2), source = (/ gen3(3,4)('ABc','DEf', 301), gen3(3,4)('GHi','JKl', 302) /)) ! tcx: (3,4) ! tcx: (3,4)

   open (unit = 1, file ='selectType002alk.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   select type (b1)
      class default
         write (1, iostat=stat, iomsg=msg )    b1
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 101_4
   end select

   select type (g => b2(1:3:2) )
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 2_4
   end select

   select type (g => b3 )
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 3_4
   end select

   select type (g => c1((/1,2/)))
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 4_4
   end select

   select type (c2)
      class default
         write (1, iostat=stat, iomsg=msg )    c2
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 5_4
   end select

   select type (g => g1)
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 6_4
   end select

   rewind 1

   msg = ''

   read (1, iostat=stat, iomsg=msg)       cc1
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 7_4
   read (1, iostat=stat, iomsg=msg)       cc2, cc3
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 8_4
   read (1, iostat=stat, iomsg=msg)       cc4, i1, cc5, i2, cc6, i3, cc7, i4
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 9_4
   read (1, iostat=stat, iomsg=msg)       cc8, cc9
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 10_4
   read (1, iostat=stat, iomsg=msg)       cc10, i5, cc11, i6, cc12, i7, cc13, i8
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 11_4
   read (1, iostat=stat, iomsg=msg)       cc14, i9, cc15, i10
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 12_4

   ! check if the values are set correctly

   if ( ( cc1 /= 'abcdef' ) .or. ( cc2 /= 'abcdef' ) .or. ( cc3 /= 'ghijkl' ) .or. &
        ( cc4 /= 'ABCDEF' ) .or. ( cc5 /= 'GHIJKL' ) .or. ( cc6 /= 'MNOPQR' ) .or. &
        ( cc7 /= 'STUVWX' ) .or. ( i1 /= 101 ) .or. ( i2 /= 102 ) .or. ( i3 /= 103 ) .or. &
        ( i4 /= 104 ) )                                 error stop 13_4

   if ( ( cc8 /= 'ABCDEF' ) .or. ( cc9 /= 'GHIJKL' ) .or. ( cc10 /= 'abcdef' ) .or. &
        ( cc11 /= 'ghijkl' ) .or. ( cc12 /= 'mnopqr' ) .or. ( cc13 /= 'stuvwx' ) .or. &
        ( i5 /= 201 ) .or. ( i6 /= 202 ) .or. ( i7 /= 203 ) .or. ( i8 /= 204 ) ) error stop 14_4

   if ( ( cc14 /= 'ABcDEf' ) .or. ( cc15 /= 'GHiJKl' ) .or. &
        ( i9 /= 301 ) .or. ( i10 /= 302 ) )   error stop 15_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
       type is (base(*)) ! tcx: (*)
          write (unit, iostat=iostat ) dtv%c
       type is (child(*)) ! tcx: (*)
          write (unit, iostat=iostat ) dtv%c, dtv%cc
       type is (gen3(*,4)) ! tcx: (*,4)
          write (unit, iostat=iostat ) dtv%c, dtv%cc, dtv%i
    end select

    iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 8 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (3,4) / declare with (*,4) - 12 changes
