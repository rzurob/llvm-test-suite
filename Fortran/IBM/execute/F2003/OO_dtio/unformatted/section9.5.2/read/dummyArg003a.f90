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
! %GROUP: dummyArg003a.f
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
!*                               - Try input item to be an unlimited polymorphic array
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

   subroutine myRead1(unit, stat, msg, a, b )
      class(*), intent(inout) :: a(:)
      class(*), intent(inout), optional :: b(:,:)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg
      
      if ( present(b) ) then
         select type (b)
            class is (base)
      	       select type (a)
                  class is (base)
                     read(unit, iostat=stat, iomsg=msg) a,b
               end select
      	 end select
      else
      	 select type (a)
            class is (base)
               read(unit, iostat=stat, iomsg=msg)  a
         end select
      end if      
        
   end subroutine

   subroutine myRead2(unit, stat, msg, a )
      class(*), intent(inout) :: a(2,*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      character(*), intent(inout) :: msg

      select type (a)
         class is (base)
            read(unit, iostat=stat, iomsg=msg) a(1:2,1)
      end select
     
   end subroutine
   
end module

program dummyArg003a
   use m1   
  
   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type(base), allocatable  :: b3(:,:)
   type(base) :: b4(2:4)                       !<= explicit shape array
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   allocate ( b1(2), source = (/ base('xxx'), base('xxx') /) )
   allocate ( b2(1,3), source = reshape ( source = (/ base('xxx'), base('xxx') , base('xxx') /), shape=(/1,3/)) )
   allocate ( b3(2,2), source = reshape ( source = (/ b1, base('xxx'), base('xxx') /), shape = (/2,2/) ) )
   b4 =(/ base('xxx'), base('xxx') , base('xxx') /)

   
   open (unit = 1, file ='dummyArg003a.data', form='unformatted', access='sequential')
   
   ! unformatted I/O operations
   
   write (1, iostat=stat, iomsg=msg )              'abcdef'
   write (1, iostat=stat, iomsg=msg )              'ABCDEFGHIJKLMNO'
   write (1, iostat=stat, iomsg=msg )              'abcdef'
   write (1, iostat=stat, iomsg=msg )              'ABCDEFGHIabcdefghijkl'
   
   write (1, iostat=stat, iomsg=msg )              'ABCDEF'
   write (1, iostat=stat, iomsg=msg )              'abcdef'
   write (1, iostat=stat, iomsg=msg )              'STUVWX'
   
   rewind 1
   
   call myRead1 (1, stat, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 1_4
      if ( ( b1(1)%c /= 'abc' ) .or. ( b1(2)%c /= 'def' ) )     error stop 2_4
      msg = ''          
             
   call myRead1 (1, stat, msg, b1, b2 )      
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 3_4
      if ( ( b1(1)%c /= 'ABC' ) .or. ( b1(2)%c /= 'DEF' )     .or.   &
           ( b2(1,1)%c /= 'GHI' ) .or. ( b2(1,2)%c /= 'JKL' ) .or.   &
           ( b2(1,3)%c /= 'MNO' )  )                            error stop 4_4
      msg = ''
           
   call myRead1 (1, stat, msg, b4(2:4:2))
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 5_4
      if ( ( b4(2)%c /= 'abc' ) .or. ( b4(3)%c /= 'xxx' ) .or.       &
           ( b4(4)%c /= 'def' ) )                               error stop 6_4
      msg = ''
      
   call myRead1 (1, stat, msg, b4, b3 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 7_4
      
      if ( ( b4(2)%c /= 'ABC' ) .or. ( b4(3)%c /= 'DEF' ) .or.       &
           ( b4(4)%c /= 'GHI' )                            .or.      &
           ( b3(1,1)%c /= 'abc' ) .or. ( b3(1,2)%c /= 'ghi' ) .or.   &
           ( b3(2,1)%c /= 'def' ) .or. ( b3(2,2)%c /= 'jkl' ) ) error stop 8_4
      msg = ''

   call myRead2 (1, stat, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 9_4
      if ( ( b1(1)%c /= 'ABC' ) .or. ( b1(2)%c /= 'DEF' ) )     error stop 10_4
      msg = ''   
      
   call myRead2 (1, stat, msg, b2(1,1:3:2) )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 11_4
      if ( ( b2(1,1)%c /= 'abc' ) .or. ( b2(1,2)%c /= 'JKL' ) .or.   &
           ( b2(1,3)%c /= 'def' )  )                            error stop 12_4
      msg = ''
      
   call myRead2 (1, stat, msg, b3(1,1:2) )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 13_4
      if ( ( b3(1,1)%c /= 'STU' ) .or. ( b3(1,2)%c /= 'VWX' ) .or.   &
           ( b3(2,1)%c /= 'def' ) .or. ( b3(2,2)%c /= 'jkl' ) ) error stop 14_4
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
