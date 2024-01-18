! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : exist002l
!*
!*  PROGRAMMER                 : David Forster (derived from exist002 by Robert Ma)
!*  DATE                       : 2007-09-28 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - EXIST= specifier: Try using INQUIRE stmt with EXIST= specifier in procedures
!*                                                     on unformatted I/O units (inquire by file)
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
         procedure, pass :: setC
   end type
   
   interface
      character(5) function doExist(file)
         character(*), intent(in) :: file
      end function
   end interface
   
contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c      
   end function   
   
   subroutine setC (a, char)
      class(base(*)), intent(inout) :: a ! tcx: (*)
      character(3), intent(in) :: char      
      a%c = char
   end subroutine
   
end module


program exist002l
   use m1
   use ISO_FORTRAN_ENV

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
   
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
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   integer :: stat1
   character(200) :: msg1

   logical :: exist1
   integer, pointer :: myUnit

   ! allocation of variables
   
   allocate (base(3)::b1,b2) ! tcx: base(3)
   allocate (myunit, source=2)
   
   b1%c = 'ibm'
   b2%c = 'ftn'

   if ( doExist('exist002l.data') /= 'DNE' )   error stop 101_4
   if ( doExist('fort.2') /= 'DNE' )          error stop 2_4
   
   open (myunit, file='exist002l.data', form='unformatted', access='direct', recl=3 )
   
   if ( doExist('exist002l.data') /= 'exist' )   error stop 3_4
   if ( doExist('fort.2') /= 'DNE' )            error stop 4_4
   
   ! try inquire statement inside DTIO
   
   write (myunit, iostat=stat1, iomsg=msg1, rec=3 ) b1
   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio write' ) ) error stop 104_4
    
   read  (myunit, iostat=stat1, iomsg=msg1, rec=3 ) b2
   

   if (( stat1 /= 0 ) .or. ( msg1 /= 'dtio read' ) ) error stop 5_4
   
   if ( b2%c /= 'ibm' ) error stop 6_4
   
   ! try to inquire preconnected units
   
   if ( doExist('exist002l.notconnected') /= 'exist' )  error stop 7_4
   
   ! close the file appropriately
   
   close ( myunit, status ='delete' )
   
   if ( doExist('exist002l.data') /= 'DNE' )   error stop 8_4
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   character(14) :: filename
   
   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    
   if ( iostat /= 0 ) error stop 9_4
       
   inquire (unit, name=filename)
    
   if ( doExist(filename) /= 'exist' ) error stop 10_4
    
   iomsg = 'dtio read'
    
   
end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   character(14) :: filename
   write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    
   if ( iostat /= 0 ) error stop 11_4
    
   FLUSH (unit, iostat=iostat, iomsg=iomsg)
   
   inquire (unit, name=filename)
    
   if ( doExist(filename) /= 'exist' ) error stop 12_4

   
   iomsg = 'dtio write'
        
end subroutine

character(5) function doExist(file)
   character(*), intent(in) ::file
   integer :: stat
   logical :: exist1 = .false.
   inquire ( file=file, exist=exist1, iostat=stat )
      
   if ( stat /= 0 ) error stop 13_4
     
   if ( exist1 ) then
      doExist = 'exist'
   else
      doExist = 'DNE'
   end if      
end function

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
