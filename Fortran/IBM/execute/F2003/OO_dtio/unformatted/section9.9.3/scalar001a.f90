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
! %GROUP: scalar001a.f
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
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - Scalar zero-sized types
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
   
   type :: base
      ! empty type
   end type
   
   type :: base1
      character(0) :: c
   end type   
      
   type :: unit
      integer(4) :: unumber
   contains
      procedure, pass :: get
   end type   
   
   interface read(unformatted)
      subroutine readUnformattedbase (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
      subroutine readUnformattedbase1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine        
   end interface
   
   interface write(unformatted)
      subroutine writeUnformattedbase (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
      
      subroutine writeUnformattedbase1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine       
   end interface  
   
   type(unit) :: u1 = unit(2)
   
contains

   integer(4) function get(a)
      class(unit), intent(in) :: a
      get = a%unumber      
   end function

end module

program scalar001a
   use m1   

   ! declaration of variables
   type(base), allocatable :: b1
   type(base), pointer     :: b2
   type(base)              :: b3
   type(base1), allocatable :: b11
   type(base1), pointer     :: b12
   type(base1)              :: b13
   
   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1
      
   ! allocation of variables
   
   allocate ( b1, b2, b11, b12 )
   
   ! I/O operations   
   
   open ( u1%get(), file = 'scalar001a.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2
   
   inquire ( iolength = length1 )  b1           !<- output list is a single scalar item
   
   if ( length1 /= 0 )             error stop 1_4
   
   inquire ( iolength = length1 )  b1, b2, b3   !<- output list is a scalar item list
   
   if ( length1 /= 0  )            error stop 2_4
      
   inquire ( iolength = length1 )  b11           !<- output list is a single scalar item
   
   if ( length1 /= 0 )             error stop 3_4
   
   inquire ( iolength = length1 )  b11, b12, b13 !<- output list is a scalar item list
   
   if ( length1 /= 0  )            error stop 4_4
      
   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b12, b3
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 5_4

   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b11, b2, b13
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 6_4
   
   rewind u1%get()
   
   read ( u1%get(), iostat=stat1, iomsg=msg1 )       b3, b11, b2
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 7_4

   read ( u1%get(), iostat=stat1, iomsg=msg1 )       b13, b1, b12
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 8_4
   
   ! close the file appropriately

   close ( u1%get(), status ='delete' )
   
end program

subroutine readUnformattedbase (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   integer :: length1   
   
   select type (dtv)
      type is (base)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 9_4
        
end subroutine


subroutine writeUnformattedbase (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base), intent(in) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg    
   
   integer :: length1   
      
   select type (dtv)
      type is (base)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 10_4
    
end subroutine

subroutine readUnformattedbase1 (dtv, myunit, iostat, iomsg)
use m1, only: base1, unit, u1
   class(base1), intent(inout) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   integer :: length1   
   
   select type (dtv)
      type is (base1)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 11_4
        
end subroutine


subroutine writeUnformattedbase1 (dtv, myunit, iostat, iomsg)
use m1, only: base1, unit, u1
   class(base1), intent(in) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg    
   
   integer :: length1   
      
   select type (dtv)
      type is (base1)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 12_4
    
end subroutine