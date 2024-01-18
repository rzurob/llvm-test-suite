! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001l
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an allocatable array with array section
!*                                 - vector subscripts, elements
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
      contains
         procedure, pass :: getC
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

end module


program array001l
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
   character(16) :: c1
   character(8)  :: c2
   character(4)  :: c3
   character(8)  :: c4

   ! allocation of variables
   allocate ( b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b2(2,2), source = reshape (source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL')  /), shape=(/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   b3 = (/ base(3)('mno'), base(3)('pqr'), base(3)('stu'), base(3)('vwx') /) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b4(2,2), source = reshape (source = (/ base(3)('MNO'), base(3)('PQR'), base(3)('STU'), base(3)('VWX')  /), shape=(/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='array001l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b1((/3,2,4,1/))    !<- writes "ghiZdefZjklZabcZ" (try array sectino with vector subscript)
   write (1, iostat=stat, iomsg=msg )             b2(1:2:1, 1:2:2)   !<- writes "ABCZDEFZ" (try array section with subscript triplet)
   write (1, iostat=stat, iomsg=msg )             b3(3)              !<- writes "stuZ" (try array element)
   write (1, iostat=stat, iomsg=msg )             b4(1, 1:2)         !<- writes "MNOZSTUZ" (try array section withe subscript triplet)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'ghiZdefZjklZabcZ' )        error stop 101_4
   if ( c2 /= 'ABCZDEFZ' )                error stop 2_4
   if ( c3 /= 'stuZ' )                    error stop 3_4
   if ( c4 /= 'MNOZSTUZ' )                error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 23 changes
