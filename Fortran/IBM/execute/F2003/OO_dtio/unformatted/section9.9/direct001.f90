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
! %GROUP: direct001.f
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
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - DIRECT= specifier: Try using INQUIRE stmt with DIRECT= specifier in procedures
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
   type, abstract :: base
      integer(4) :: x
   contains
      procedure, nopass :: gettype => gettypebase
      procedure, pass   :: getX
      procedure(inf), pass, deferred :: getY
      procedure, pass   :: setX
      procedure(inf2), pass, deferred :: setY
   end type
   
   type, extends(base) :: child
      real(4) :: y
   contains
      procedure, nopass :: gettype => gettypechild
      procedure, pass   :: getY
      procedure, pass   :: setY
   end type
   
   type :: unit
      integer(4) :: unumber
   contains
      procedure, pass :: get
      procedure, pass :: isDirect
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
   
   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface  
   
   interface
      real function inf(a)
         import base
         class(base), intent(in) :: a
      end function
   end interface

   interface
      subroutine inf2(a, i)
         import base
         class(base), intent(inout) :: a
         real(4) :: i
      end subroutine
   end interface
      
   type(unit) :: u1 = unit(2)
   
contains

   integer(4) function gettypebase()
      gettypebase = 1
   end function

   integer(4) function gettypechild()
      gettypechild = 2
   end function

   integer(4) function get(a)
      class(unit), intent(in) :: a
      get = a%unumber      
   end function
      
   character(1) function isDirect(a)
      class(unit), intent(in) :: a
      character(1) :: tmp
      inquire ( a%get(), direct=tmp )
      
      if ( tmp .eq. 'Y' ) then
         isDirect = 'T'
      else if ( tmp .eq. 'N' ) then
         isDirect = 'F'
      else if ( tmp .eq. 'U' ) then
      	 isDirect = 'U'
      else 
      	 isDirect = 'E'
      end if       
   end function
   
   integer(4) function getX(a)
      class(base), intent(in) :: a
      getX = a%x      
   end function
   
   real(4) function getY(a)
      class(child), intent(in) :: a
      getY = a%y     
   end function
   
   subroutine setX(a, i)
      class(base), intent(inout) :: a
      integer(4) :: i
      a%x = i
   end subroutine
   
   subroutine setY(a, i)
      class(child), intent(inout) :: a
      real(4) :: i
      a%y = i
   end subroutine

end module


program direct001
   use m1   

   ! declaration of variables
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   logical :: precision_r4
   
   character(10) :: direct1 
   integer(4) :: iolength1
   
   character(200) :: msg1 = ''
   integer :: stat1
      
   ! allocation of variables
   
   allocate ( b1, source = child(x=1,y=2.0) )
   allocate ( b2, source = child(x=3,y=4.0) )
   
   ! I/O operations   
   
   inquire ( u1%get(), direct = direct1) 
   
   if ( direct1 /= 'UNKNOWN' )     error stop 1_4
   
   inquire ( iolength = iolength1 )  child(1,2.0)   !<- get the size of child()
   
   open ( u1%get(), file = 'direct001.data', form='unformatted', access='direct', recl= iolength1)   !<- open a external file with unit #2
   
   if ( u1%isDirect() /= 'T' )      error stop 2_4
   
   write ( u1%get(), iostat=stat1, iomsg=msg1, rec = 1 ) b1
   if ( stat1 /= 0 ) error stop 3_4
   write ( u1%get(), iostat=stat1, iomsg=msg1, rec = 2 ) b2
   if ( stat1 /= 0 ) error stop 4_4
   
   read  ( u1%get(), iostat=stat1, iomsg=msg1, rec = 1 ) b2
   if ( stat1 /= 0 ) error stop 5_4
   
   read  ( u1%get(), iostat=stat1, iomsg=msg1, rec = 2 ) b1  
   if ( stat1 /= 0 ) error stop 6_4
   
   ! check if the values are read/write correctly

   if ( ( b1%getx() /= 3 ) .or. (.not. precision_r4(b1%gety(),4.0) ) ) error stop 7_4
   if ( ( b2%getx() /= 1 ) .or. (.not. precision_r4(b2%gety(),2.0) ) ) error stop 8_4
   
   ! close the file appropriately

   close ( u1%get(), status ='delete' )
   
end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4) :: tmp1 = 0
   integer(4) :: tmp2 = 0
   
   integer(4) :: tmpi1
   real(4) :: tmpr2
   
   if ( u1%isDirect() /= 'T' )   error stop 9_4
   
   read ( u1%get(), iostat=tmp1, iomsg=iomsg ) tmpi1
   
   call dtv%setX(tmpi1)
   
   associate( mydtv => dtv )
      if ( mydtv%gettype() == 2 ) then !<- dynamic type is child
         read ( u1%get(), iostat=tmp2, iomsg=iomsg ) tmpr2
         call mydtv%setY(tmpr2)    	
      end if   
   end associate
   
   iostat = tmp1 + tmp2
        
end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base), intent(in) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg    

   integer(4) :: tmp1 = 0
   integer(4) :: tmp2 = 0
     
   if ( u1%isDirect() /= 'T' )   error stop 10_4
   
   write ( u1%get(), iostat=tmp1, iomsg=iomsg ) dtv%getX()
   
   associate( mydtv => dtv )
      if ( mydtv%gettype() == 2 ) then !<- dynamic type is child
         write ( u1%get(), iostat=tmp2, iomsg=iomsg ) mydtv%getY()    	
      end if   
   end associate
   
   iostat = tmp1 + tmp2
    
end subroutine