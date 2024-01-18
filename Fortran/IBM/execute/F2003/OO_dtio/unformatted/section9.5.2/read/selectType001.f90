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
! %GROUP: selectType001.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate name (from select type construct)
!*                               Direct Access
!*                               
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
      procedure, pass :: get => getc      
   end type
   
   type, extends(base) :: child
      character(3) :: cc = ''
   contains
      procedure, pass :: get => getcc
   end type

contains

   function getc (a)
      character(3) :: getc
      class(base), intent(in) :: a
      getc = a%c
   end function
   function getcc (a)
      character(3) :: getcc
      class(child), intent(in) :: a
      getcc = a%cc
   end function
end module


program selectType001
   use m1   

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
  
   ! declaration of variables
   class(base), pointer     :: b1
   class(base), allocatable :: b2
   class(base),  allocatable :: b3
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   
   allocate ( b1, source = child('xxx', 'xxx') )
   allocate ( b2, source = child('xxx','xxx') )
   allocate ( b3, source = base('xxx') )
      
   open (unit = 1, file ='selectType001.data', form='unformatted', access='direct', recl = 6)
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg, rec=1 )         'abcdef'
   write (1, iostat=stat, iomsg=msg, rec=3 )         'ghijkl'
   write (1, iostat=stat, iomsg=msg, rec=2 )         'mno'

   
   select type (b11 => b1)
      class is (base)
         read (1, iostat=stat, iomsg=msg, rec=1 )    b11     !<= write b11%c and b11%cc
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 1_4
         if ( b11%c      /= 'abc' )                     error stop 2_4
         if ( b11%get()  /= 'def' )                     error stop 3_4    !<- get() calls getcc()
         msg = ''
      class default
         error stop 4_4
   end select
   
   select type (b12 => b2)
      class is (child)
         read (1, iostat=stat, iomsg=msg, rec=3 )    b12     !<= write b12%c and b12%cc
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 5_4   
         if ( b12%c      /= 'ghi' )                     error stop 6_4
         if ( b12%get()  /= 'jkl' )                     error stop 7_4    !<- get() calls getcc()
         msg = ''
      class default
         error stop 8_4
   end select

   select type (b13 => b3)
      class is (base)
         read (1, iostat=stat, iomsg=msg, rec=2 )    b13     !<= write only b13%c
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 9_4   
         if ( b13%get()      /= 'mno' )                 error stop 10_4    !<- get() calls getc()
         msg = ''
      class default
         error stop 11_4
   end select
   
   if ( b1%c      /= 'abc' )                     error stop 12_4
   if ( b1%get()  /= 'def' )                     error stop 13_4    !<- get() calls getcc()
   if ( b2%c      /= 'ghi' )                     error stop 14_4
   if ( b2%get()  /= 'jkl' )                     error stop 15_4    !<- get() calls getcc()
   if ( b3%get()  /= 'mno' )                     error stop 16_4    !<- get() calls getc()         
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    
    if ( iostat /= 0 ) error stop 16_4
    
    select type (dtv)
       type is (child)
          read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

   iomsg = 'dtio'
       
end subroutine
