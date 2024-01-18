! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : end004akl
!*
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.10: Error, end-of-record, and end-of-file conditions
!*                               - If an end-of-file condition occurs during execution of an input/output
!*                                 statement that contains enither an END= nor and IOSTAT= specifier,
!*                                 execution of the program is terminated (stream access)
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

   type, abstract :: base (lbase_1) ! lbase_1=1
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type

   type, extends(base) :: child (kchild_1) ! kchild_1=4
      integer, kind :: kchild_1
      integer(kchild_1) :: cc = -1
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


program end004akl
   use m1
   use ISO_FORTRAN_ENV

   ! declaration of variables
   class(base(:)), allocatable :: b1(:) ! tcx: (:)

   integer :: stat
   character(200) :: msg

   ! allocation of variables

   allocate (b1(3), source = (/ child(1,4)('a',1), child(1,4)('b',2), child(1,4)('c',3) /) ) ! tcx: (1,4) ! tcx: (1,4) ! tcx: (1,4)

   open (unit = 1, file ='end004akl.1', form='unformatted', access='stream')

   ! unformatted I/O operations

   write ( 1, iostat = stat, iomsg = msg )     b1(1:3:2)

   rewind 1

   print *, "Before Program termination"
   read ( 1, iomsg = msg, pos = 1 )            b1       !<- end of file reached and no iostat= nor end=, program terminates here
   print *, "ERROR"

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iomsg=iomsg, iostat=iostat ) dtv%c

   select type(dtv)
      type is (child(*,4)) ! tcx: (*,4)
         read (unit, iomsg=iomsg, iostat=iostat ) dtv%cc
   end select

end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg, iostat=iostat ) dtv%c

   select type(dtv)
      type is (child(*,4)) ! tcx: (*,4)
         write (unit, iomsg=iomsg, iostat=iostat ) dtv%cc
   end select

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (1) / declare with (*) - 5 changes
! type: child - added parameters (kchild_1) to invoke with (1,4) / declare with (*,4) - 5 changes
