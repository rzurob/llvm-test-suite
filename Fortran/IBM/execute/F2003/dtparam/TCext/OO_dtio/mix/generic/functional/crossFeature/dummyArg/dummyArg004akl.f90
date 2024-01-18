!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg004akl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg004a by Robert Ma)
!*  DATE                       : 2007-08-07 (original: 04/26/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Dummy Argument Association
!*                                    - Dummy Argument being non-polymorphic assumed-size array entity with unformatted i/o
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

   type base (lb) ! lb=3
      integer, len :: lb
      character(lb) :: c = 'xxx'
      contains
         procedure, pass :: write
         procedure, pass :: read
         generic :: write(unformatted) => write
         generic :: read(unformatted)  => read
   end type

   interface
      subroutine mywriteext ( dtv )
         import base
         type(base(*)), intent(in) :: dtv(1,*) ! tcx: (*)
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv )
         import base
         type(base(*)), intent(inout) :: dtv(2,*) ! tcx: (*)
      end subroutine
   end interface

   contains

      subroutine write (dtv, unit, iostat, iomsg)
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine read (dtv, unit, iostat, iomsg)
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtioreadb'

      end subroutine

      subroutine mywrite ( dtv )
         type(base(*)), intent(in) :: dtv(1,*) ! tcx: (*)
         integer :: stat
         character(200) :: msg

         write ( 1, iostat = stat, iomsg = msg )           dtv(1,1:4)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

      end subroutine

      subroutine myread ( dtv )
         type(base(*)), intent(inout) :: dtv(2,*) ! tcx: (*)
         integer :: stat
         character(200) :: msg

         read ( 1, iostat = stat, iomsg = msg )           dtv(1:2,1:2)
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 2_4

      end subroutine

end module

program dummyArg004akl
   use m

   type(base(3))              :: b1(4) = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /)  ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   type(base(:)), allocatable :: b2(:,:) ! tcx: (:)
   type(base(:)), allocatable :: b3(:) ! tcx: (:)

   open ( 1, file='dummyArg004akl.1', form='unformatted', access='sequential' )

   allocate ( b2(2,2), source = reshape ( source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /), shape = (/2,2/) ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b3(4), source = (/ base(3)('mno'), base(3)('pqr'), base(3)('stu'), base(3)('vwx') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   call mywrite ( b1 )
   call mywrite ( b2 )
   call mywrite ( b3 )

   call mywriteext ( b3 )
 
   rewind 1

   call myread  ( b2 )
   call myread  ( b3 )
   call myread  ( b1 )

   if ( ( b1(1)%c /= 'mno' ) .or. ( b1(2)%c /= 'pqr' ) .or. ( b1(3)%c /= 'stu' ) .or. ( b1(4)%c /= 'vwx' ) .or. &
        ( b2(1,1)%c /= 'abc' ) .or. ( b2(2,1)%c /= 'def' ) .or. ( b2(1,2)%c /= 'ghi' ) .or. ( b2(2,2)%c /= 'jkl' ) .or. &
        ( b3(1)%c /= 'ABC' ) .or. ( b3(2)%c /= 'DEF' ) .or. ( b3(3)%c /= 'GHI' ) .or. ( b3(4)%c /= 'JKL' ) )     error stop 3_4

   call myreadext ( b2 )

   if ( ( b2(1,1)%c /= 'mno' ) .or. ( b2(2,1)%c /= 'pqr' ) .or. ( b2(1,2)%c /= 'stu' ) .or. ( b2(2,2)%c /= 'vwx' )  )     error stop 4_4
   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv )
   use m, only: base
   type(base(*)), intent(in) :: dtv(1,*) ! tcx: (*)
   integer :: stat
   character(200) :: msg

   write ( 1, iostat = stat, iomsg = msg ) dtv(1,1:4)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 5_4

end subroutine

subroutine myreadext ( dtv )
   use m, only: base
   type(base(*)), intent(inout) :: dtv(2,*) ! tcx: (*)
   integer :: stat
   character(200) :: msg

   read ( 1, iostat = stat, iomsg = msg ) dtv(1:2,1:2)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 23 changes
