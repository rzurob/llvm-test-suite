!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg001akl
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg001a by Robert Ma)
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
!*                                    - Dummy Argument being non-polymorphic scalar entity with unformatted i/o
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
      subroutine mywriteext ( dtv, j )
         import base
         type(base(:)), pointer, intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: j
      end subroutine
   end interface

   interface
      subroutine myreadext ( dtv, j )
         import base
         type(base(:)), allocatable, intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: j
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

      subroutine mywrite ( dtv, j )
         type(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: j
         integer :: stat
         character(200) :: msg

         write ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 1_4

      end subroutine

      subroutine myread ( dtv, j )
         type(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: j
         integer :: stat
         character(200) :: msg

         read ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
         if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 2_4

      end subroutine

end module

program dummyArg001
   use m

   type(base(3))              :: b1 = base(3) ('abc') ! tcx: (3) ! tcx: (3)
   type(base(:)), allocatable :: b2 ! tcx: (:)
   type(base(:)), pointer     :: b3 ! tcx: (:)

   open ( 1, file='dummyArg001.1', form='unformatted', access='direct', recl=10 )

   allocate ( b2, source = base(3)('def') ) ! tcx: (3)
   allocate ( b3, source = base(3)('ghi') ) ! tcx: (3)

   call mywrite ( b1, 6 )
   call mywrite ( b2, 1 )
   call mywrite ( b3, 5 )

   call mywriteext ( b3, 10 )

   call myread  ( b1, 5 )
   call myread  ( b2, 6 )
   call myread  ( b3, 1 )

   if ( ( b1%c /= 'ghi' ) .or. ( b2%c /= 'abc' ) .or. ( b3%c /= 'def' ) ) error stop 3_4

   call myreadext ( b2, 10 )

   if ( b2%c /= 'ghi' ) error stop 4_4   

   close ( 1, status ='delete')

end program

subroutine mywriteext ( dtv, j )
   use m, only: base
   type(base(:)), pointer, intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: j
   integer :: stat
   character(200) :: msg

   write ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) ) error stop 5_4

end subroutine

subroutine myreadext ( dtv, j )
   use m, only: base
   type(base(:)), allocatable, intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: j
   integer :: stat
   character(200) :: msg

   read ( 1, iostat = stat, iomsg = msg, rec = j ) dtv
   if ( ( stat /= 0 ) .or. ( msg /= 'dtioreadb' ) ) error stop 6_4

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (3) / declare with (*) - 14 changes
