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
! %GROUP: scalar002.f
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
!*                               - inquire iolength of scalar polymorphic items 
!*                                 when polymorphic items are in "type is" type guard
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
      integer(8)   :: x
      real(4)      :: y
      character(3) :: z
   contains
      procedure, pass   :: getX
      procedure, pass   :: getY
      procedure, pass   :: getZ
      procedure, pass   :: setX
      procedure, pass   :: setY
      procedure, pass   :: setZ
   end type
   
   type, extends(base) :: child
      integer(2) :: a
      real(8)    :: b
      complex(8) :: c
   end type
   
   type :: unit
      integer(4) :: unumber
   contains
      procedure, pass :: get
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
   
   type(unit) :: u1 = unit(2)
   
contains

   integer(4) function get(a)
      class(unit), intent(in) :: a
      get = a%unumber      
   end function
   
   integer(8) function getX(a)
      class(base), intent(in) :: a
      getX = a%x      
   end function
   
   real(4) function getY(a)
      class(base), intent(in) :: a
      getY = a%y     
   end function
   
   character(3) function getZ(a)
      class(base), intent(in) :: a
      getZ = a%z      
   end function
   
   subroutine setX(a, i)
      class(base), intent(inout) :: a
      integer(8) :: i
      a%x = i
   end subroutine
   
   subroutine setY(a, i)
      class(base), intent(inout) :: a
      real(4) :: i
      a%y = i
   end subroutine
   
   subroutine setZ(a, i)
      class(base), intent(inout) :: a
      character(3) :: i
      a%z = i
   end subroutine

end module

program scalar002
   use m1   

   ! declaration of variables
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   class(base), allocatable :: b3
   class(base), pointer     :: b4  

   logical :: precision_r4
   
   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1
      
   ! allocation of variables
   
   allocate ( b1, source = base(x=1,y=2.0,z='ibm') )
   allocate ( b2, source = child(x=3,y=4.0,z='ftn', a=5, b=6.0, c=(7.0,8.0)) )
   allocate ( b3, b4 )
   
   ! I/O operations   
   
   open ( u1%get(), file = 'scalar002.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2
   
   select type (b1)
      type is (base) 
         inquire ( iolength = length1 )   b1           !<- output list is a single scalar item
      class default
         error stop 1_4
   end select

   if ( length1 /= 16 )             error stop 2_4
   
   select type (b2)
      type is (base) 
         error stop 3_4
      type is (child)
         inquire ( iolength = length1 )   b2           !<- output list is a single scalar item
      class default
         error stop 4_4
   end select
 
   if ( length1 /= 48 )             error stop 5_4   
   
   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b2   
   
   rewind u1%get()
   
   read  ( u1%get(), iostat=stat1, iomsg=msg1 )      b3, b4
   
   
   ! check if values are read correctly
   
   if (( b3%x /= 1 ) .or. (.not. precision_r4(b3%y, 2.0)) .or. ( b3%z /= 'ibm' ) )   error stop 6_4
   if (( b4%x /= 3 ) .or. (.not. precision_r4(b4%y, 4.0)) .or. ( b4%z /= 'ftn' ) )   error stop 7_4
   
   ! close the file appropriately

   close ( u1%get(), status ='delete' )
   
end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   integer :: length1
   
   select type ( a => dtv )
      type is (base)
         inquire ( iolength = length1 ) a
         if ( length1 /= 16 )    error stop 8_4         
         read ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z
      type is (child)
         inquire ( iolength = length1 ) a
         if ( length1 /= 48 )    error stop 9_4
         read ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z, a%a, a%b, a%c
      class default
         error stop 10_4
   end select
           
end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
use m1, only: base, child, unit, u1
   class(base), intent(in) :: dtv
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg    

   select type ( a => dtv )
      type is (base)
         inquire ( iolength = length1 ) a
         if ( length1 /= 16 )    error stop 11_4         
         write ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z
      type is (child)
         inquire ( iolength = length1 ) a
         if ( length1 /= 48 )    error stop 12_4
         write ( u1%get(), iostat=iostat, iomsg=iomsg ) a%x, a%y, a%z, a%a, a%b, a%c
      class default
         error stop 13_4
   end select
    
end subroutine
