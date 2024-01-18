!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: final001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Finalization process
!*                               Try DTIO inside the finalization process
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
   type base
      character(3) :: c
   contains
      procedure, pass :: getC
      procedure, pass :: setC
      final :: finalbase, finalbaserank1
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
      
contains

   character(3) function getC(dtv)
      class(base), intent(in) :: dtv
      getC = dtv%c
   end function
   
   subroutine setC(dtv, c)
      class(base), intent(inout) :: dtv
      character(3) :: c
      dtv%c = c
   end subroutine
   
   subroutine finalbase(dtv)
      type(base), intent(in) :: dtv
      integer :: stat
      character(200) :: msg
      
      write (unit = 3, iostat=stat, iomsg=msg)   dtv
      
      if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 1_4
      
   end subroutine      

   subroutine finalbaserank1(dtv)
      type(base), intent(in) :: dtv(:)
      integer :: stat
      character(200) :: msg

      write (unit = 3, iostat=stat, iomsg=msg)   dtv
      
      if ( (stat /= 0 ) .or. ( msg /= 'dtiowrite') )   error stop 2_4
      
   end subroutine 
  
end module


program final001
   use m1   
  
   ! declaration of variables
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base) , allocatable :: b3
   class(base), pointer     :: b4(:)
   
   integer :: stat
   character(200) :: msg

   open (unit = 3, file ='final001.3', form='unformatted', access='stream')   
   
   ! allocation of variables
   
   allocate (b1, source = base('abc') )           !<- finalized
   allocate (b2, source = base('def') )           !<- finalized
   allocate (b3, source = base('ghi') )           !<- finalized
   allocate (b4(3), source = (/ b3, b2, b1 /) )   !<- no finalization here!
      
   ! unformatted I/O operations
   
   read ( 3, iostat = stat, iomsg = msg, pos=1 )   b2
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 3_4
   read ( 3, iostat = stat, iomsg = msg, pos=4 )   b3
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 4_4
   read ( 3, iostat = stat, iomsg = msg, pos=7 )   b1
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 5_4
   read ( 3, iostat = stat, iomsg = msg, pos=1 )   b4
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 6_4
      
   if ( b2%getC() /= 'abc' )            error stop 7_4
   if ( b3%getC() /= 'def' )            error stop 8_4
   if ( b1%getC() /= 'ghi' )            error stop 9_4
   if ( ( b4(1)%getC() /= 'abc' ) .or. ( b4(2)%getC() /= 'def' ) .or. &
        ( b4(3)%getC() /= 'ghi' ) )     error stop 10_4
      
   deallocate (b1)  !<- finalized       write at pos 10
   deallocate (b2)  !<- finalized       write at pos 13
   deallocate (b3)  !<- finalized       write at pos 16
   deallocate (b4)  !<- finalized       write at pos 19 - 27
   
   allocate ( b1, b2, b3, b4(3) )
   
   read ( 3, iostat = stat, iomsg = msg, pos=10 )   b4
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 11_4
   read ( 3, iostat = stat, iomsg = msg, pos=19 )   b1
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 12_4
   read ( 3, iostat = stat, iomsg = msg, pos=22 )   b2
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 13_4
   read ( 3, iostat = stat, iomsg = msg, pos=25 )   b3
      if ( (stat /= 0 ) .or. ( msg /= 'dtioread') )   error stop 14_4    
   
   if ( b1%getC() /= 'abc' )            error stop 15_4
   if ( b2%getC() /= 'def' )            error stop 16_4
   if ( b3%getC() /= 'ghi' )            error stop 17_4
   if ( ( b4(1)%getC() /= 'ghi' ) .or. ( b4(2)%getC() /= 'abc' ) .or. &
        ( b4(3)%getC() /= 'def' ) )     error stop 18_4   

   ! close the file appropriately

   close ( 3, status ='delete' )   
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
    
   read (unit,  iostat=iostat ) dtv%c

   iomsg = 'dtioread'
      
end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1,  only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iostat=iostat ) dtv%c
   
   iomsg = 'dtiowrite'

end subroutine
