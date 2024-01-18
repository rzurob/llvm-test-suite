! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : zerosized001lk
!*
!*  PROGRAMMER                 : David Forster (derived from zerosized001 by Robert Ma)
!*  DATE                       : 2007-09-10 (original: 11/04/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Ensure zero-sized type will invoke DTIO (sequential access)
!*                                        and ensure zero-length records work with DTIO
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type :: base (lbase_1) ! lbase_1=0
       integer, len :: lbase_1
       character(lbase_1) :: i 
    end type
       
    type :: emptybase (keb) ! keb=1
       integer, kind :: keb
    end type    
    
end module

program zerosized001lk
use m

   interface read(unformatted)
        
      subroutine unformattedRead (dtv, unit, iostat, iomsg)
      use m
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
       
      subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase(1)), intent(inout) :: dtv ! tcx: (1)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface
    
   interface write(unformatted)
      subroutine unformattedWrite (dtv, unit, iostat, iomsg)
      use m
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
        
      subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
      use m
         class(emptybase(1)), intent(in) :: dtv ! tcx: (1)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface
   
   ! declaration of variables
   
   integer :: stat
   
   character(200) :: msg  = ''
   character(1) :: c1, c2, c3, c4, c5
   
   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)
   type (base(0))              :: b3 ! tcx: (0)
   
   class(emptybase(1)), allocatable :: e1 ! tcx: (1)
   class(emptybase(1)), pointer     :: e2 ! tcx: (1)
   type (emptybase(1))              :: e3 ! tcx: (1)
   
   ! allocation of variables
   
   allocate (base(0):: b1, b2 ) ! tcx: base(0)
   allocate ( e1, e2 )
   
   open(1, file='zerosized001lk.data', access='sequential', form='unformatted')
   
   write (1, iostat = stat, iomsg = msg) b1
      if ( ( stat /= 998 ) .or. ( msg /= '' ) ) error stop 101_4
   write (1, iostat = stat, iomsg = msg) "A"
   write (1, iostat = stat, iomsg = msg) e1
      if ( ( stat /= 996 ) .or. ( msg /= '' ) ) error stop 2_4
   write (1, iostat = stat, iomsg = msg) "B"      
   write (1, iostat = stat, iomsg = msg) b2
      if ( ( stat /= 998 ) .or. ( msg /= '' ) ) error stop 3_4
   write (1, iostat = stat, iomsg = msg) "C"
   write (1, iostat = stat, iomsg = msg) e2
      if ( ( stat /= 996 ) .or. ( msg /= '' ) ) error stop 4_4
   write (1, iostat = stat, iomsg = msg) "D"
   write (1, iostat = stat, iomsg = msg) b3
      if ( ( stat /= 998 ) .or. ( msg /= '' ) ) error stop 5_4
   write (1, iostat = stat, iomsg = msg) "E"      
   write (1, iostat = stat, iomsg = msg) e3
      if ( ( stat /= 996 ) .or. ( msg /= '' ) ) error stop 6_4   
   
   rewind 1
   
   read (1, iostat = stat, iomsg = msg) b1
      if ( ( stat /= 999 ) .or. ( msg /= '' ) ) error stop 7_4
   read (1, iostat = stat, iomsg = msg) c1
   read (1, iostat = stat, iomsg = msg) e1
      if ( ( stat /= 997 ) .or. ( msg /= '' ) ) error stop 8_4
   read (1, iostat = stat, iomsg = msg) c2      
   read (1, iostat = stat, iomsg = msg) b2
      if ( ( stat /= 999 ) .or. ( msg /= '' ) ) error stop 9_4
   read (1, iostat = stat, iomsg = msg) c3
   read (1, iostat = stat, iomsg = msg) e2
      if ( ( stat /= 997 ) .or. ( msg /= '' ) ) error stop 10_4
   read (1, iostat = stat, iomsg = msg) c4
   read (1, iostat = stat, iomsg = msg) b3
      if ( ( stat /= 999 ) .or. ( msg /= '' ) ) error stop 11_4
   read (1, iostat = stat, iomsg = msg) c5     
   read (1, iostat = stat, iomsg = msg) e3
      if ( ( stat /= 997 ) .or. ( msg /= '' ) ) error stop 12_4
   
   ! check if the values are read properly
   
   if ( c1 /= "A" ) error stop 13_4
   if ( c2 /= "B" ) error stop 14_4
   if ( c3 /= "C" ) error stop 15_4
   if ( c4 /= "D" ) error stop 16_4
   if ( c5 /= "E" ) error stop 17_4
      
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   
end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m

   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   read (unit, iomsg=iomsg ) dtv%i    !<- zero length character
   
   iostat = 999    ! change the iostat so that we know DTIO is called

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write (unit, iomsg=iomsg ) dtv%i    !<- zero length character
   
   iostat = 998    ! change the iostat so that we know DTIO is called

end subroutine

subroutine unformattedReadZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase(1)), intent(inout) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   select type (dtv) 
      type is (emptybase(1)) ! tcx: (1)
         read (unit, iomsg=iomsg) dtv
   end select
   
   iostat = 997    ! change the iostat so that we know DTIO is called

end subroutine


subroutine unformattedWriteZero (dtv, unit, iostat, iomsg)
use m
   class(emptybase(1)), intent(in) :: dtv ! tcx: (1)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   select type (dtv) 
      type is (emptybase(1)) ! tcx: (1)
         write (unit, iomsg=iomsg) dtv
   end select
   
   iostat = 996    ! change the iostat so that we know DTIO is called

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (0) / declare with (*) - 7 changes
! type: emptybase - added parameters (keb) to invoke with (1) / declare with (1) - 9 changes
! type: base - added parameters (lbase_1) to invoke with (0) / declare with (*) - 7 changes
! type: emptybase - added parameters (keb) to invoke with (1) / declare with (1) - 9 changes
