! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : integer004kl
!*
!*  PROGRAMMER                 : David Forster (derived from integer004 by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.6.1.1: Integer Editing
!*                                        Try different integer editing descriptor in child data transfer stmt
!*                                        First try B, O, X, and I with m == 0 and data is also 0.  (only blanks should be written)
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
   type fourintegers (kf1,kf2,kf3,kf4) ! kf1,kf2,kf3,kf4=1,2,4,8
      integer, kind :: kf1,kf2,kf3,kf4
      integer(kf1) :: c1 = 0_kf1
      integer(kf2) :: c2 = 0_kf2
      integer(kf3) :: c3 = 0_kf3
      integer(kf4) :: c4 = 0_kf4
   end type  
end module


program integer004kl
   use m1   
   
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import fourintegers
         class(fourintegers(1,2,4,8)), intent(in) :: dtv ! tcx: (1,2,4,8)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
  
   ! declaration of variables

   class(fourintegers(1,2,4,8)), allocatable :: f1 ! tcx: (1,2,4,8)
   class(fourintegers(1,2,4,8)), pointer     :: f2 ! tcx: (1,2,4,8)
   type(fourintegers(1,2,4,8)) , allocatable :: f3(:) ! tcx: (1,2,4,8)
   type(fourintegers(1,2,4,8)) , pointer     :: f4(:,:) ! tcx: (1,2,4,8)
         
   integer :: stat
   character(200) :: msg
   character(32)  :: m2, m3
   character(96)  :: m4
   character(192) :: m5
   
   integer :: size1   
   
   allocate (f1)
   allocate (f2)
   
   allocate ( f3(3), source = (/f1, f2, f1/) )
   allocate ( f4(2,2), source = reshape( source=(/f3, f1/), shape=(/2,2/)) )
      
   open (unit = 1, file ='integer004kl.1', form='formatted', access='sequential')
   open (unit = 2, file ='integer004kl.2', form='formatted', access='stream', status='replace' )   
      
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)             f1
   write (2, *, iostat=stat, iomsg=msg)             f2
   
   inquire ( 1, size= size1 ) 
   if ( size1 /= 33 ) error stop 1_4    !<- 1 space in the beginning, and 1 carriage return at the end of record
   
   inquire ( 2, size= size1 ) 
   if ( size1 /= 33 ) error stop 2_4    !<- 1 space in the beginning, and 1 carriage return at the end of record
  
   rewind 1
   rewind 2
   
   read ( 1, "(A)" ) m2
   read ( 2, "(A)" ) m3
   
   if ( m2 /= '                                ' ) error stop 3_4
   if ( m3 /= '                                ' ) error stop 4_4
   
   rewind 1
   rewind 2   

   write (1, *, iostat=stat, iomsg=msg)             f3
   write (2, *, iostat=stat, iomsg=msg)             f1, f2, f4  

   inquire ( 1, size= size1 ) 
   if ( size1 /= 97 )  error stop 5_4    !<- 1 space in the beginning of each effective item, and 1 carriage return at the end of record
   
   inquire ( 2, size= size1 ) 
   if ( size1 /= 193 ) error stop 6_4    !<- 1 space in the beginning of each effective item, and 1 carriage return at the end of record
   
   rewind 1
   rewind 2  
   
   read ( 1, "(A)" ) m4
   read ( 2, "(A)" ) m5
   
   if ( m4 /= '                                                                                                ' ) error stop 3_4
   if ( m5 /= '                                                                                                                                                                                                ' ) error stop 4_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   close ( 2, status ='delete' )   

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1
   class(fourintegers(1,2,4,8)), intent(in) :: dtv ! tcx: (1,2,4,8)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: format
   integer :: stat1, stat2, stat3, stat4
   
10 format (I2.0)
20 format (1X,B6.0)
   format = "(1X,O19.0)"
    
   write (unit, 10, iomsg=iomsg, iostat=stat1 )              dtv%c1
   write (unit, fmt=format, iomsg=iomsg, iostat=stat2 )      dtv%c2
   write (unit, 20, iomsg=iomsg, iostat=stat3 )              dtv%c3
   write (unit, fmt="(1X,Z0.0)", iomsg=iomsg, iostat=stat4 ) dtv%c4

   if ( ( stat1 /= 0 ) .or. ( stat2 /= 0 ) .or. ( stat3 /= 0 ) .or. ( stat4 /= 0 ) )        error stop 7_4

end subroutine



! Extensions to introduce derived type parameters:
! type: fourintegers - added parameters (kf1,kf2,kf3,kf4) to invoke with (1,2,4,8) / declare with (1,2,4,8) - 6 changes
