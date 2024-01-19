! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be io-implied-do
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

   type,extends(base) :: child
      character(lbase_1) :: c1 = ''
   end type

end module


program implieddo001l
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
   class(base(:)), allocatable     :: b1(:) ! tcx: (:)
   class(base(:)), allocatable     :: b2(:,:) ! tcx: (:)
   type(base(3)) :: b3(4) ! tcx: (3)
   type(base(:)),  pointer :: b4(:,:) ! tcx: (:)
   integer :: stat
   character(200) :: msg
   character(12)  :: c1
   character(24)  :: c2
   character(6)   :: c3
   character(3)   :: c4

   ! allocation of variables
   allocate ( b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape (source = (/ child(3)('ABC','abc'), child(3)('DEF','def'), child(3)('GHI','ghi'), child(3)('JKL','jkl')  /), shape=(/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   b3 = (/ base(3)('mno'), base(3)('pqr'), base(3)('stu'), base(3)('vwx') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b4(2,2), source = reshape (source = (/ base(3)('MNO'), base(3)('PQR'), base(3)('STU'), base(3)('VWX')  /), shape=(/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='implieddo001l.1', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             ( b1, i= 1, 1, -1 )   !<- iteration count is 1
   write (1, iostat=stat, iomsg=msg )             ( ( b2(i,j), i = 1, 2), j = 1, 2 )
   write (1, iostat=stat, iomsg=msg )             ( b3(k), k=4_4,2_2,-2_8 )
   write (1, iostat=stat, iomsg=msg )             ( b4(2,2), j=1,2,3 )

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly
   if ( c1 /= 'abcdefghijkl' )                            error stop 101_4
   if ( c2 /= 'ABCabcDEFdefGHIghiJKLjkl' )                error stop 2_4
   if ( c3 /= 'vwxpqr' )                                  error stop 3_4
   if ( c4 /= 'VWX' )                                     error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    select type ( dtv )
       type is (child(*)) ! tcx: (*)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%c1
    end select

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 18 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 5 changes
