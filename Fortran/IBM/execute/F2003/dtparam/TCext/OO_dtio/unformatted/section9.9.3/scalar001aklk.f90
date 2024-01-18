! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scalar001aklk
!*
!*  PROGRAMMER                 : David Forster (derived from scalar001a by Robert Ma)
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
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
   
   type :: base (kbase) ! kbase=1
      integer, kind :: kbase
      ! empty type
   end type
   
   type :: base1 (lbase1_1) ! lbase1_1=0
      integer, len :: lbase1_1
      character(lbase1_1) :: c
   end type   
      
   type :: unit (kunit_1) ! kunit_1=4
      integer, kind :: kunit_1
      integer(kunit_1) :: unumber
   contains
      procedure, pass :: get
   end type   
   
   interface read(unformatted)
      subroutine readUnformattedbase (dtv, unit, iostat, iomsg)
         import base
         class(base(1)), intent(inout) :: dtv ! tcx: (1)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
      subroutine readUnformattedbase1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine        
   end interface
   
   interface write(unformatted)
      subroutine writeUnformattedbase (dtv, unit, iostat, iomsg)
         import base
         class(base(1)), intent(in) :: dtv ! tcx: (1)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
      
      subroutine writeUnformattedbase1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine       
   end interface  
   
   type(unit(4)) :: u1 = unit(4)(2) ! tcx: (4) ! tcx: (4)
   
contains

   integer(4) function get(a)
      class(unit(4)), intent(in) :: a ! tcx: (4)
      get = a%unumber      
   end function

end module

program scalar001aklk
   use m1   

   ! declaration of variables
   type(base(1)), allocatable :: b1 ! tcx: (1)
   type(base(1)), pointer     :: b2 ! tcx: (1)
   type(base(1))              :: b3 ! tcx: (1)
   type(base1(:)), allocatable :: b11 ! tcx: (:)
   type(base1(:)), pointer     :: b12 ! tcx: (:)
   type(base1(0))              :: b13 ! tcx: (0)
   
   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1
      
   ! allocation of variables
   
   allocate ( b1, b2 )
   allocate (base1(0):: b11, b12 ) ! tcx: base1(0)
   
   ! I/O operations   
   
   open ( u1%get(), file = 'scalar001aklk.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2
   
   inquire ( iolength = length1 )  b1           !<- output list is a single scalar item
   
   if ( length1 /= 0 )             error stop 101_4
   
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
   class(base(1)), intent(inout) :: dtv ! tcx: (1)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   integer :: length1   
   
   select type (dtv)
      type is (base(1)) ! tcx: (1)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 9_4
        
end subroutine


subroutine writeUnformattedbase (dtv, myunit, iostat, iomsg)
use m1, only: base, unit, u1
   class(base(1)), intent(in) :: dtv ! tcx: (1)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg    
   
   integer :: length1   
      
   select type (dtv)
      type is (base(1)) ! tcx: (1)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 10_4
    
end subroutine

subroutine readUnformattedbase1 (dtv, myunit, iostat, iomsg)
use m1, only: base1, unit, u1
   class(base1(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   integer :: length1   
   
   select type (dtv)
      type is (base1(*)) ! tcx: (*)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 11_4
        
end subroutine


subroutine writeUnformattedbase1 (dtv, myunit, iostat, iomsg)
use m1, only: base1, unit, u1
   class(base1(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: myunit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg    
   
   integer :: length1   
      
   select type (dtv)
      type is (base1(*)) ! tcx: (*)
         inquire ( iolength = length1 ) dtv
   end select
   
   if ( length1 /= 0 ) error stop 12_4
    
end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase) to invoke with (1) / declare with (1) - 9 changes
! type: base1 - added parameters (lbase1_1) to invoke with (0) / declare with (*) - 9 changes
! type: unit - added parameters (kunit_1) to invoke with (4) / declare with (4) - 11 changes
