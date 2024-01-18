! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array001akk
!*
!*  PROGRAMMER                 : David Forster (derived from array001a by Robert Ma)
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
!*                               - array non-polymorphic zero-sized type
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
   end type
      
   type :: unit (kunit_1) ! kunit_1=4
      integer, kind :: kunit_1
      integer(kunit_1) :: unumber
   contains
      procedure, pass :: get
   end type   
   
   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(1)), intent(inout) :: dtv ! tcx: (1)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
   
   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(1)), intent(in) :: dtv ! tcx: (1)
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

program array001akk
   use m1   

   ! declaration of variables
   type(base(1)), allocatable :: b1(:) ! tcx: (1)
   type(base(1)), pointer     :: b2(:,:) ! tcx: (1)
   type(base(1))              :: b3(3) ! tcx: (1)
   
   logical :: precision_r4
   
   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1
      
   ! allocation of variables
   
   allocate ( b1(5), source = (/ ( base(1)(), i=1,5) /) ) ! tcx: (1)
   allocate ( b2(2,2), source = reshape ( source = (/ ( base(1)(),i=1,4) /), shape = (/2,2/))) ! tcx: (1)
   b3       = (/ ( base(1)(), i=1,3) /)  ! tcx: (1)
   
   ! I/O operations   
   
   open ( u1%get(), file = 'array001akk.data', form='unformatted', access='sequential' )   !<- open a external file with unit #2
   
   inquire ( iolength = length1 )  b1           !<- output list is a single array item
   
   if ( length1 /= 0 )            error stop 101_4
   
   inquire ( iolength = length1 )  b2           !<- output list is a single array item
   
   if ( length1 /= 0 )            error stop 2_4
   
   inquire ( iolength = length1 )  b3           !<- output list is a single array item
   
   if ( length1 /= 0 )            error stop 3_4   
   
   inquire ( iolength = length1 )  b1,b2,b3     !<- output list is a array item list
   
   if ( length1 /= 0 )            error stop 4_4     
   
   inquire ( iolength = length1 )  b1(1:3:2),b2(2:1:-1,1),b3((/3,1/))     !<- output list is a array section list
   
   if ( length1 /= 0 )            error stop 5_4 
   
   write ( u1%get(), iostat=stat1, iomsg=msg1 )      b1, b2, b3
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 6_4
   
   rewind u1%get()
   
   read ( u1%get(), iostat=stat1, iomsg=msg1 )       b3, b1, b2
   if ( (stat1 /= 0) .or. (msg1 /= '') )    error stop 7_4
   
   ! close the file appropriately

   close ( u1%get(), status ='delete' )
   
end program

subroutine readUnformatted (dtv, myunit, iostat, iomsg)
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
   
   if ( length1 /= 0 ) error stop 11_4
        
end subroutine


subroutine writeUnformatted (dtv, myunit, iostat, iomsg)
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
   
   if ( length1 /= 0 ) error stop 12_4
   
end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters () to invoke with () / declare with () - 12 changes
! type: base - added parameters (kbase) to invoke with (1) / declare with (1) - 12 changes
! type: unit - added parameters (kunit_1) to invoke with (4) / declare with (4) - 7 changes
