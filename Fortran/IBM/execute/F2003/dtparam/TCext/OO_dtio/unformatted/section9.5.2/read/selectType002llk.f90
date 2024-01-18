! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-19 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read selector in class default in select type construct
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
      integer(kgen3_1) :: i = -999
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
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

   ! allocation of variables

   allocate ( b1, source = base(3) () ) ! tcx: (3)
   allocate ( b2, source = child(3,3)() ) ! tcx: (3,3)
   allocate ( b3, source = gen3(3,3,4) () ) ! tcx: (3,3,4)

   allocate ( c1, source = child(3,3)() ) ! tcx: (3,3)
   allocate ( c2, source = gen3(3,3,4)() ) ! tcx: (3,3,4)

   allocate ( g1, source = gen3(3,3,4)() ) ! tcx: (3,3,4)
   allocate ( g2, source = gen3(3,3,4)() ) ! tcx: (3,3,4)

   open (unit = 1, file ='selectType002llk.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write ( 1, iostat = stat, iomsg = msg)     'abc'
   write ( 1, iostat = stat, iomsg = msg)     'def', 'ghi'
   write ( 1, iostat = stat, iomsg = msg)     'jkl', 'mno', 101
   write ( 1, iostat = stat, iomsg = msg)     'ABC', 'DEF'
   write ( 1, iostat = stat, iomsg = msg)     'GHI', 'JKL', 201
   write ( 1, iostat = stat, iomsg = msg)     'ABc', 'DEf', 301
   write ( 1, iostat = stat, iomsg = msg)     'GHi', 'JKl', 302

   rewind 1
   msg = ''

   select type (g => b1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 101_4
         if ( g%c /= 'abc' )                           error stop 2_4
   end select

   select type (b2)
      class default
         read (1, iostat=stat, iomsg=msg )    b2
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 3_4
   end select

   select type (b3)
      class default
         read (1, iostat=stat, iomsg=msg )    b3
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 4_4
   end select

   select type (g => c1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 5_4
         if ( ( g%c /= 'ABC' ) .or. (g%cc /= 'DEF' ) ) error stop 6_4
   end select

   select type (c2)
      class default
         read (1, iostat=stat, iomsg=msg )    c2
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 7_4
   end select

   select type (g => g1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( g%c /= 'ABc' ) .or. (g%cc /= 'DEf' ) .or. (g%i /= 301 ) ) error stop 8_4
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 9_4
   end select

   select type (g2)
      class default
         read (1, iostat=stat, iomsg=msg )    g2
         if ( ( g2%c /= 'GHi' ) .or. (g2%cc /= 'JKl' ) .or. (g2%i /= 302 ) ) error stop 10_4
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 11_4
   end select

   ! check if the values are set correctly

   select type ( b2 )
      type is (child(*,*)) ! tcx: (*,*)
         if ( ( b2%c /= 'def' ) .or. ( b2%cc /= 'ghi' ) ) error stop 12_4
      class default
         error stop 13_4
   end select

   select type ( b3 )
      type is (gen3(*,*,4)) ! tcx: (*,*,4)
         if ( ( b3%c /= 'jkl' ) .or. ( b3%cc /= 'mno' ) .or. ( b3%i /= 101 ) ) error stop 14_4
      class default
         error stop 15_4
   end select

   select type ( c2 )
      type is (gen3(*,*,4)) ! tcx: (*,*,4)
         if ( ( c2%c /= 'GHI' ) .or. ( c2%cc /= 'JKL' ) .or. ( c2%i /= 201 ) ) error stop 16_4
      class default
         error stop 17_4
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
       type is (base(*)) ! tcx: (*)
          read (unit, iostat=iostat ) dtv%c
       type is (child(*,*)) ! tcx: (*,*)
          read (unit, iostat=iostat ) dtv%c, dtv%cc
       type is (gen3(*,*,4)) ! tcx: (*,*,4)
          read (unit, iostat=iostat ) dtv%c, dtv%cc, dtv%i
    end select

    iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 6 changes
! type: gen3 - added parameters (kgen3_1) to invoke with (3,3,4) / declare with (*,*,4) - 9 changes
