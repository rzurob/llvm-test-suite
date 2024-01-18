! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg002al
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an array dummy argument of pointer/allocatable
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
      class(base(:)), intent(in), allocatable :: a(:) ! tcx: (*)
      class(base(*)), intent(in), optional, pointer :: b(:,:) ! tcx: (*)
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

program dummyArg002al
   use m1

   ! declaration of variables
   class(base(:)), allocatable, dimension(:) :: b1 ! tcx: (:)
   class(base(3)), pointer, dimension(:,:)   :: b2 ! tcx: (:)

   integer :: stat
   character(200) :: msg
   character(8)  :: c1
   character(20)  :: c2

   ! allocation of variables
   allocate ( b1(2), source = (/ base(3)('abc'), base(3)('def') /) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2(1,3), source = reshape ( source = (/ base(3)('ABC'), base(3)('DEF') , base(3)('GHI') /), shape=(/1,3/)) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='dummyArg002al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   call myWrite (1, stat, msg, b1 )                !<- write 'abcZdefZ' to file
   call myWrite (1, stat, msg, b1, b2 )            !<- write 'abcZdefZABCZDEFZGHIZ' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2

   ! check if the values are set correctly

   if ( c1 /= 'abcZdefZ' )                          error stop 101_4
   if ( c2 /= 'abcZdefZABCZDEFZGHIZ' )              error stop 2_4

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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 12 changes
