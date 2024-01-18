! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg001l
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an scalar dummy argument
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

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function

   subroutine myWrite(unit, stat, msg, a, b )
      class(base(*)), intent(in) :: a ! tcx: (*)
      class(base(*)), intent(in), optional :: b ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      if (.not. present(b) ) then
         write(unit, iostat=stat, iomsg=msg) a
      else
      	 write(unit, iostat=stat, iomsg=msg) a,b
      end if
   end subroutine

end module

program dummyArg001l
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type(base(:)), allocatable  :: b3 ! tcx: (:)
   type(base(:)), pointer      :: b4 ! tcx: (:)
   integer :: stat
   character(200) :: msg
   character(4)  :: c1
   character(8)  :: c2
   character(4)  :: c3
   character(8) :: c4

   ! allocation of variables
   allocate ( b1, source = base(3)('ibm') ) ! tcx: (3)
   allocate ( b2, source = base(3)('ftn') ) ! tcx: (3)
   allocate ( b3, source = b1 )             !<- 'ibm'
   allocate ( b4, source = b2 )             !<- 'ftn'


   open (unit = 1, file ='dummyArg001l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWrite (1, stat, msg, b1 )                !<- write 'ibmZ' to file
   call myWrite (1, stat, msg, b1, b2 )            !<- write 'ibmZftnZ' to file
   call myWrite (1, stat, msg, b3 )                !<- write 'ibmZ' to file
   call myWrite (1, stat, msg, b3, b4 )            !<- write 'ibmZftnZ' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'ibmZ' )                  error stop 101_4
   if ( c2 /= 'ibmZftnZ' )              error stop 2_4
   if ( c3 /= 'ibmZ' )                  error stop 3_4
   if ( c4 /= 'ibmZftnZ' )              error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 11 changes
