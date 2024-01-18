! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : pointer001l
!*
!*  PROGRAMMER                 : David Forster (derived from pointer001 by Robert Ma)
!*  DATE                       : 2007-09-17 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: 
!*                               if input is a pointer, data are transferred from file to 
!*                               the associated target.  If an output is a pointer, data shall
!*                               transfer from target to file. (WRITE, scalar pointer)
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


program pointer001l
   use m1   

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
   
  
   ! declaration of variables
   class(base(:)), pointer :: b1, b2 ! tcx: (:)
   class(base(:)), pointer :: b3, b5 ! tcx: (:)
   type(base(:)),  allocatable, target :: b4, b6 ! tcx: (:)
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   allocate ( b3, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b4, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b5, source = base(3)('xxx') ) ! tcx: (3)
   allocate ( b6, source = base(3)('xxx') ) ! tcx: (3)
   
   b1 => b3
   b2 => b4
   
   open (unit = 1, file ='pointer001l.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )                'abc'
   write (1, iostat=stat, iomsg=msg )                'defghi'
   write (1, iostat=stat, iomsg=msg )                'ABC'
   write (1, iostat=stat, iomsg=msg )                'DEFGHI'
   
   rewind 1
   
   read (1, iostat=stat, iomsg=msg )                 b1         !<- shall read the content to b3('abc') from file
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 101_4   
      if ( b1%c /= 'abc' )                           error stop 2_4
      if ( b3%c /= 'abc' )                           error stop 3_4
      msg = ''
      
   read (1, iostat=stat, iomsg=msg )                 b2, b1     !<- shall read the content to b4 and b3 ('def' and 'ghi') from file
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 4_4   
      if ( b1%c /= 'ghi' )                           error stop 5_4
      if ( b2%c /= 'def' )                           error stop 6_4
      if ( b3%c /= 'ghi' )                           error stop 7_4
      if ( b4%c /= 'def' )                           error stop 8_4
      msg = ''
           
   ! change pointer association target           
   b1 => b5 
   b2 => b6    
   
   read (1, iostat=stat, iomsg=msg )                 b1         !<- shall read the content to b5('ABC') from file
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 9_4   
      if ( b1%c /= 'ABC' )                           error stop 10_4
      if ( b3%c /= 'ghi' )                           error stop 11_4      !<- NO CHANGE
      if ( b5%c /= 'ABC' )                           error stop 12_4
      msg = ''
   
   read (1, iostat=stat, iomsg=msg )                 b2, b1     !<- shall read the content to b6 and b5 ('DEF' and 'GHI') from file   
      if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 13_4   
      if ( b1%c /= 'GHI' )                           error stop 14_4
      if ( b2%c /= 'DEF' )                           error stop 15_4
      if ( b3%c /= 'ghi' )                           error stop 16_4      !<- NO CHANGE
      if ( b4%c /= 'def' )                           error stop 17_4      !<- NO CHANGE
      if ( b5%c /= 'GHI' )                           error stop 18_4
      if ( b6%c /= 'DEF' )                           error stop 19_4
      msg = ''
      
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp 
   read (unit, iostat=iostat ) temp
   
   dtv%c = temp
   
   iomsg = 'dtio'

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 9 changes
