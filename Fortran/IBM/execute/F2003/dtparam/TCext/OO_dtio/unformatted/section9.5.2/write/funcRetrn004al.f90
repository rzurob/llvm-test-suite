! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn004al
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn004a by Robert Ma)
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be a non-polymorphic array function return (pointer, allocatable, and neither)
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

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

contains

   function getNewBase(c)
      type(base(3)) :: getNewBase(2) ! tcx: (3)
      character(3) :: c
      getNewBase = base(3)(c) ! tcx: (3)
   end function
   
   function getNewChild(c)
      type(child(3)) :: getNewChild(3) ! tcx: (3)
      character(6) :: c
      getNewChild = child(3)(c(1:3),c(4:6)) ! tcx: (3)
   end function

end module

program funcRetrn004al
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
   
   interface
      function getNewBasePtr(c)
         import base
         type(base(:)), pointer :: getNewBasePtr(:) ! tcx: (:)
         character(3) :: c
      end function
   end interface
   
   interface   
      function getNewChildAlloc(c)
         import child
         type(child(:)), allocatable :: getNewChildAlloc(:) ! tcx: (:)
         character(6) :: c
      end function
   end interface
   
   ! declaration of variables
   integer :: stat
   character(200) :: msg
   character(6)   :: c1
   character(18)  :: c2
   character(6)   :: c3
   character(12)  :: c4
    
   open (unit = 1, file ='funcRetrn004al.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )             getNewBase('abc')
   write (1, iostat=stat, iomsg=msg )             getNewChild('defghi')
   write (1, iostat=stat, iomsg=msg )             getNewBasePtr('jkl')
   write (1, iostat=stat, iomsg=msg )             getNewChildAlloc('mnopqr')
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   
   ! check if the values are set correctly

   if ( c1 /= 'abcabc' )                   error stop 101_4
   if ( c2 /= 'defghidefghidefghi' )       error stop 2_4
   if ( c3 /= 'jkljkl' )                   error stop 3_4
   if ( c4 /= 'mnopqrpqrmno' )             error stop 4_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
  
end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base(*)) ! tcx: (*)
         write (unit, iostat=iostat) dtv%c
      type is (child(*)) ! tcx: (*)
         write (unit, iostat=iostat) dtv%c, dtv%cc
   end select
   iomsg = 'dtiowrite'
end subroutine

function getNewBasePtr(c)
   use m1
   type(base(:)), pointer :: getNewBasePtr(:) ! tcx: (:)
   character(3) :: c
   allocate ( getNewBasePtr(2), source = (/ base(3)(c), base(3)(c) /) ) ! tcx: (3) ! tcx: (3)
end function
   
function getNewChildAlloc(c)
   use m1
   type(child(:)), allocatable :: getNewChildAlloc(:) ! tcx: (:)
   character(6) :: c
   allocate(getNewChildAlloc(2), source = (/ child(3)(c(1:3),c(4:6)), child(3)(c(4:6),c(1:3)) /) ) ! tcx: (3) ! tcx: (3)
 end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 7 changes
