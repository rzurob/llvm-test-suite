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
! %GROUP: stream001a.f
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
!*                               - STREAM= specifier: Try using INQUIRE stmt with STREAM= specifier in procedures
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
      real(4) :: x
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
      procedure, pass :: isStream
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
      
   logical function isStream(a)
      class(unit), intent(in) :: a
      character(1) :: tmp
      inquire ( a%get(), stream=tmp )
      
      if ( tmp .eq. 'Y' ) then
         isStream = .true.
      else
         isStream = .false.      
      end if       
   end function
   
   real(4) function getX(a)
      class(base), intent(in) :: a
      getX = a%x      
   end function
   
   real(4) function getY(a)
      class(child), intent(in) :: a
      getY = a%y     
   end function
   
   subroutine setX(a, i)
      class(base), intent(inout) :: a
      real(4) :: i
      a%x = i
   end subroutine
   
   subroutine setY(a, i)
      class(child), intent(inout) :: a
      real(4) :: i
      a%y = i
   end subroutine

end module


program stream001a
   use m1   

   ! declaration of variables
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:)
   logical :: precision_r4
   
   character(200) :: msg1 = ''
   integer :: stat1
      
   ! allocation of variables
   
   allocate ( b1(2), source = (/ child(x=1.0,y=2.0), child(x=1.0,y=2.0) /) )
   allocate ( b2(0), source = (/ (child(1.0,2.0), i=1, 0) /) )
   
   ! I/O operations   
   
   if ( u1%isStream() )      error stop 1_4   !<- initially, unit is not defined with stream access, stream= specifier shall set variable to UNKNOWN
   
   open ( u1%get(), file = 'stream001a.data', form='unformatted', access='direct', recl=16 )   !<- open a external file with unit #2
   
   if ( u1%isStream() )      error stop 2_4
   
   write ( u1%get(), iostat=stat1, iomsg=msg1, rec=11 ) b1
   if ( stat1 /= 0 ) error stop 3_4
   write ( u1%get(), iostat=stat1, iomsg=msg1, rec=12 ) b2
   if ( stat1 /= 0 ) error stop 4_4
   
   deallocate (b2)
   allocate( b2(2), source = (/ (child(0.0,0.0), i=1, 2) /)  )
   
   read  ( u1%get(), iostat=stat1, iomsg=msg1, rec=11 ) b2
   if ( stat1 /= 0 ) error stop 5_4
   
   read  ( u1%get(), iostat=stat1, iomsg=msg1, rec=11) b1  
   if ( stat1 /= 0 ) error stop 6_4
   
   ! check if the values are read/write correctly

   if ( (.not. precision_r4(b1(1)%getx(),1.0) ) .or. (.not. precision_r4(b1(1)%gety(),2.0) ) ) error stop 7_4
   if ( (.not. precision_r4(b1(2)%getx(),1.0) ) .or. (.not. precision_r4(b1(2)%gety(),2.0) ) ) error stop 8_4
   if ( (.not. precision_r4(b2(1)%getx(),1.0) ) .or. (.not. precision_r4(b2(1)%gety(),2.0) ) ) error stop 9_4
   if ( (.not. precision_r4(b2(2)%getx(),1.0) ) .or. (.not. precision_r4(b2(2)%gety(),2.0) ) ) error stop 10_4
   
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
   
   real(4) :: tmpr1, tmpr2
   
   if ( u1%isStream() )   error stop 11_4
   
   read ( u1%get(), iostat=tmp1, iomsg=iomsg ) tmpr1
   
   call dtv%setX(tmpr1)
   
   associate( mydtv => dtv )
      if ( mydtv%gettype() == 2 ) then !<- dynamic type is child
         read ( u1%get(), iostat=tmp2, iomsg=iomsg ) tmpr2
         call dtv%setY(tmpr2)    	
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
     
   if ( u1%isStream() )   error stop 12_4
   
   write ( u1%get(), iostat=tmp1, iomsg=iomsg ) dtv%getX()
   
   associate( mydtv => dtv )
      if ( mydtv%gettype() == 2 ) then !<- dynamic type is child
         write ( u1%get(), iostat=tmp2, iomsg=iomsg ) dtv%getY()    	
      end if   
   end associate
   
   iostat = tmp1 + tmp2
    
end subroutine
