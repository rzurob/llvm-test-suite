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
! %GROUP: dummyArg003.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an unlimited polymorphic scalar variable
!*                               Direct Access
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
      character(3) :: c = ''
      contains
         procedure, pass :: getC
   end type
   
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

   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c      
   end function   

   subroutine myRead(unit, stat, recn, msg, a, b )
      class(*) :: a
      class(*), optional :: b
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: recn
      character(*), intent(inout) :: msg
      
      if ( present(b) ) then
         select type (b)
            class is (base)
      	       select type (a)
                  class is (base)
                     read(unit, iostat=stat, iomsg=msg, rec=recn) a,b
               end select
      	 end select
      else
      	 select type (a)
            class is (base)
               read(unit, iostat=stat, iomsg=msg, rec=recn)  a
         end select
      end if     
        
   end subroutine
   
end module

program dummyArg003
   use m1   
  
   ! declaration of variable
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(base), allocatable  :: b3
   type(base) :: b4(2:4)                       !<= explicit shape array
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   allocate ( b1, source = base('xxx') )
   allocate ( b2, source = base('xxx') )
   allocate ( b3, source = base('xxx') )
   b4 =(/ base('xxx'), base('xxx') , base('xxx') /)

   
   open (unit = 1, file ='dummyArg003.data', form='unformatted', access='direct', recl=6)
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg, rec=4 )              'abc'
   write (1, iostat=stat, iomsg=msg, rec=2 )              'ABCDEF'
   write (1, iostat=stat, iomsg=msg, rec=3 )              'def'
   write (1, iostat=stat, iomsg=msg, rec=1 )              'DEFGHI'
   
   call myRead (1, stat, 4, msg, b1 )    
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 1_4
      if ( b1%c /= 'abc')                                       error stop 2_4
      msg = ''   
      
   call myRead (1, stat, 2, msg, b1, b2 )   
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 3_4
      if ( ( b1%c /= 'ABC' ) .or. ( b2%c /= 'DEF' )  )          error stop 4_4
      msg = ''   
      
   call myRead (1, stat, 3, msg, b3)    
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 5_4
      if ( b3%c /= 'def')                                       error stop 6_4
      msg = ''    
      
   call myRead (1, stat, 1, msg, b4(2), b3 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 7_4
      if ( ( b4(2)%c /= 'DEF' ) .or. ( b4(3)%c /= 'xxx' ) .or.  &
           ( b4(4)%c /= 'xxx' )                           .or.  &
           ( b3%c    /= 'GHI' ) )                               error stop 8_4
      msg = '' 
      
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp 
   read (unit, iostat=iostat ) temp
   
   dtv%c = temp
   
   iomsg = 'dtio'

end subroutine
