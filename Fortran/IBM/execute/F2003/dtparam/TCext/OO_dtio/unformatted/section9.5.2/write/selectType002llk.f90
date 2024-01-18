! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : selectType002llk
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write selector in class default in select type construct
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

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
   end type

end module


module m1
   use m

   type, extends(child) :: gen3 (kgen3_1) ! kgen3_1=4
      integer, kind :: kgen3_1
      integer(kgen3_1) :: i
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

program selectType002llk
   use m1

   ! declaration of variables
   class(base(:)), pointer     :: b1 ! tcx: (:)
   class(base(:)), allocatable :: b2, b3 ! tcx: (:)

   class(child(:,:)), pointer    :: c1 ! tcx: (:,:)
   class(child(:,:)), allocatable:: c2 ! tcx: (:,:)

   class(gen3(:,:,4)), pointer     :: g1 ! tcx: (:,:,4)
   class(gen3(:,:,4)), allocatable :: g2 ! tcx: (:,:,4)

   integer :: stat
   character(200) :: msg =''

   character(3) :: cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9, cc10, cc11, cc12, cc13
   integer(4)   :: i1, i2, i3, i4

   ! allocation of variables

   allocate ( b1, source = base(3) ('abc') ) ! tcx: (3)
   allocate ( b2, source = child(3,3)('def','ghi') ) ! tcx: (3,3)
   allocate ( b3, source = gen3(3,3,4) ('jkl','mno', 101 ) ) ! tcx: (3,3,4)

   allocate ( c1, source = child(3,3)('ABC', 'DEF') ) ! tcx: (3,3)
   allocate ( c2, source = gen3(3,3,4)('GHI', 'JKL', 201 ) ) ! tcx: (3,3,4)

   allocate ( g1, source = gen3(3,3,4)('ABc','DEf', 301) ) ! tcx: (3,3,4)
   allocate ( g2, source = gen3(3,3,4)('GHi','JKl', 302) ) ! tcx: (3,3,4)

   open (unit = 1, file ='selectType002llk.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   select type (g => b1)
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 101_4
   end select

   select type (b2)
      class default
         write (1, iostat=stat, iomsg=msg )    b2
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 2_4
   end select

   select type (b3)
      class default
         write (1, iostat=stat, iomsg=msg )    b3
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 3_4
   end select

   select type (g => c1)
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

   select type (g2)
      class default
         write (1, iostat=stat, iomsg=msg )    g2
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 7_4
   end select

   rewind 1

   msg = ''

   read (1, iostat=stat, iomsg=msg)       cc1
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 8_4
   read (1, iostat=stat, iomsg=msg)       cc2, cc3
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 9_4
   read (1, iostat=stat, iomsg=msg)       cc4, cc5, i1
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 10_4
   read (1, iostat=stat, iomsg=msg)       cc6, cc7
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 11_4
   read (1, iostat=stat, iomsg=msg)       cc8, cc9, i2
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 12_4
   read (1, iostat=stat, iomsg=msg)       cc10, cc11, i3
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 13_4
   read (1, iostat=stat, iomsg=msg)       cc12, cc13, i4
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 14_4


   ! check if the values are set correctly

   if ( ( cc1 /= 'abc' ) .or. ( cc2 /= 'def' ) .or. ( cc3 /= 'ghi' ) .or. &
        ( cc4 /= 'jkl' ) .or. ( cc5 /= 'mno' ) .or. ( i1 /= 101 ) ) error stop 15_4

   if ( ( cc6 /= 'ABC' ) .or. ( cc7 /= 'DEF' ) .or. ( cc8 /= 'GHI' ) .or. &
        ( cc9 /= 'JKL' ) .or. ( i2 /= 201 ) )                       error stop 16_4

   if ( ( cc10 /= 'ABc' ) .or. ( cc11 /= 'DEf' ) .or. ( cc12 /= 'GHi' ) .or. &
        ( cc13 /= 'JKl' ) .or. ( i3 /= 301 ) .or. ( i4 /= 302 ) )   error stop 17_4

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
       type is (child(*,*)) ! tcx: (*,*)
          write (unit, iostat=iostat ) dtv%c, dtv%cc
       type is (gen3(*,*,4)) ! tcx: (*,*,4)
          write (unit, iostat=iostat ) dtv%c, dtv%cc, dtv%i
    end select

    iomsg = 'dtiowrite'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 5 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (3,3,4) / declare with (*,*,4) - 7 changes
