! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArg002l
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArg002 by Robert Ma)
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be an array dummy argument (assumed-shape)
!*                               Stream Access
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
   end type
   
   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
   
contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c      
   end function   

   subroutine myRead1(unit, stat, posn, msg, a, b )
      class(base(*)), intent(inout) :: a(:) ! tcx: (*)
      class(base(*)), intent(inout), optional :: b(:,:) ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: posn
      character(*), intent(inout) :: msg
      
      if (.not. present(b) ) then
         read(unit, iostat=stat, iomsg=msg, pos=posn) a
      else
      	 read(unit, iostat=stat, iomsg=msg, pos=posn) a,b
      end if       
   end subroutine

   subroutine myRead2(unit, stat, posn, msg, a )
      class(base(*)), intent(inout) :: a(2,*) ! tcx: (*)
      integer, intent(in)  :: unit
      integer, intent(out) :: stat
      integer, intent(in)  :: posn
      character(*), intent(inout) :: msg
            
      read (unit, iostat=stat, iomsg=msg, pos=posn) a(1:2,1)
     
   end subroutine
   
end module

program dummyArg002l
   use m1   
  
   ! declaration of variables
   class(base(:)), allocatable :: b1(:) ! tcx: (:)
   class(base(:)), pointer     :: b2(:,:) ! tcx: (:)
   type(base(:)), allocatable  :: b3(:,:) ! tcx: (:)
   type(base(3)) :: b4(2:4)                       !<= explicit shape array ! tcx: (3)
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   allocate ( b1(2), source = (/ base(3)('xxx'), base(3)('xxx') /) ) ! tcx: (3) ! tcx: (3)
   allocate ( b2(1,3), source = reshape ( source = (/ base(3)('xxx'), base(3)('xxx') , base(3)('xxx') /), shape=(/1,3/)) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b3(2,2), source = reshape ( source = (/ b1, base(3)('xxx'), base(3)('xxx') /), shape = (/2,2/) ) ) ! tcx: (3) ! tcx: (3)
   b4 =(/ base(3)('xxx'), base(3)('xxx') , base(3)('xxx') /) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   
   open (unit = 1, file ='dummyArg002l.data', form='unformatted', access='stream')
   
   ! unformatted I/O operations
   
   write (1, iostat = stat, iomsg = msg, pos = 61)         'abcdef'
   write (1, iostat = stat, iomsg = msg, pos = 46)         'ABCDEFabcdefghi'
   write (1, iostat = stat, iomsg = msg, pos = 40)         'STUWXY'
   write (1, iostat = stat, iomsg = msg, pos = 19)         'ABCDEFGHIabcdefghijkl'
   
   write (1, iostat = stat, iomsg = msg, pos = 13)         'abcdef'
   write (1, iostat = stat, iomsg = msg, pos = 7 )         'ABCDEF'
   write (1, iostat = stat, iomsg = msg, pos = 1 )         'ghijkl'
   
   call myRead1 (1, stat, 61, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 101_4
      if ( ( b1(1)%c /= "abc" ) .or. ( b1(2)%c /= "def" ) )     error stop 2_4
      msg = ''
      
   call myRead1 (1, stat, 46, msg, b1, b2 )
      if ( ( stat /= 0 ) .or. ( msg /= 'dtio' ) )               error stop 3_4
      if ( ( b1(1)%c /= "ABC" ) .or. ( b1(2)%c /= "DEF" )       .or.  &
           ( b2(1,1)%c /= "abc" ) .or. ( b2(1,2)%c /= "def" )   .or.  &
           ( b2(1,3)%c /= "ghi" ) )                             error stop 4_4
      msg = ''   
   
   call myRead1 (1, stat, 40, msg, b4(2:4:2))
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 5_4
      if ( ( b4(2)%c /= "STU" ) .or. ( b4(3)%c /= "xxx" ) .or.  &
           ( b4(4)%c /= "WXY" ) )                               error stop 6_4
      msg = ''
      
   call myRead1 (1, stat, 19, msg, b4, b3 )
      if ( ( stat /= 0 ) .or. ( msg /= 'dtio' ) )               error stop 7_4
      if ( ( b4(2)%c /= "ABC" ) .or. ( b4(3)%c /= "DEF" )       .or.  &
           ( b4(4)%c /= "GHI" )                                 .or.  &
           ( b3(1,1)%c /= "abc" ) .or. ( b3(2,1)%c /= "def" )   .or.  &
           ( b3(1,2)%c /= "ghi" ) .or. ( b3(2,2)%c /= "jkl" ))  error stop 8_4
      msg = ''      
   
   ! dummy argument of myRead2 is an assumed-size array
   
   call myRead2 (1, stat, 13, msg, b1 )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 9_4
      if ( ( b1(1)%c /= "abc" ) .or. ( b1(2)%c /= "def" ) )     error stop 10_4
      msg = ''
      
   call myRead2 (1, stat, 7, msg, b2(1,1:3:2) )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 11_4
      if ( ( b2(1,1)%c /= "ABC" ) .or. ( b2(1,2)%c /= "def" )   .or.  &         
           ( b2(1,3)%c /= "DEF" ) )                             error stop 12_4   !<- b2(1,2) shall have no change
      msg = ''
      
   call myRead2 (1, stat, 1, msg, b3(1,1:2) )
      if ( (stat /= 0) .or. (msg /= 'dtio') )                   error stop 13_4
      if ( ( b3(1,1)%c /= "ghi" ) .or. ( b3(2,1)%c /= "def" )   .or.  &
           ( b3(1,2)%c /= "jkl" ) .or. ( b3(2,2)%c /= "jkl" ))  error stop 14_4   !<- b3(2,1) and b3(2,2) shall have no change
      msg = ''     

   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(3) :: temp 
   read (unit, iostat=iostat ) temp
   
   dtv%c = temp
   
   iomsg = 'dtio'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 20 changes
