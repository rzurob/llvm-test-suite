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
!*                               - Try output item function return of some transformational intrinsic functions
!*                                 including cshift, eoshift, merge
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
end module

program funcRetrn002l
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
   character(16)   :: c1, c2, c3

   class(base(:)), allocatable :: b1(:), b2(:,:) ! tcx: (:)
   class(base(:)), pointer :: b3(:), b4(:,:)    ! tcx: (:)
   logical :: mergemask(2,2) = reshape( source = (/ .true. , .false. , .true. , .false. /), shape = (/2,2/) )

   ! allocation of variables

   allocate(b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b2(2,2), source = reshape( source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b3(4), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b4(2,2), source = reshape( source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='funcRetrn002l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             cshift (b1, shift=-1)                        !<- write 'jklabcdefghi' to file (try intrinsic cshift)
   write (1, iostat=stat, iomsg=msg )             eoshift(b3,shift=-1, boundary=base(3)('xxx') )  !<- write 'xxxABCDEFGHI' to file (try intrinsic eoshift) ! tcx: (3)
   write (1, iostat=stat, iomsg=msg )             merge (b2,b4,mergemask)                      !<- write 'abcDEFghiJKL' to file (try intrinsic merge)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3


   ! check if the values are set correctly

   if ( c1 /= 'jklZabcZdefZghiZ' )                  error stop 101_4
   if ( c2 /= 'xxxZABCZDEFZGHIZ' )                  error stop 2_4
   if ( c3 /= 'abcZDEFZghiZJKLZ' )                  error stop 3_4

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
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 21 changes
