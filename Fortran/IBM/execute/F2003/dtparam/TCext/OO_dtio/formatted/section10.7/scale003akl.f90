! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scale003akl
!*
!*  PROGRAMMER                 : David Forster (derived from scale003a by Robert Ma)
!*  DATE                       : 2007-07-23 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.7.5: P Editing
!*                                        Inside DTIO, P edit and see if the connection has changed scale factor temporarily for read with E and D editor
!*                                        
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

   type :: base (kb,lb) ! kb,lb=4,2
      integer, kind :: kb
      integer, len :: lb
      real(kb)      :: r1(lb)
      complex(kb)   :: c1(lb)
   end type
         
   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

end module

program scale003akl
   use m1   

   ! declaration of variables

   class(base(4,:)), allocatable  :: f1 ! tcx: (4,:)
   type(base(4,:)) , allocatable  :: f3 ! tcx: (4,:)
   class(base(4,:)), pointer      :: f4(:) ! tcx: (4,:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'scale003akl.1', form='formatted', access='sequential' )
   
   ! allocation of variables
   
   allocate (base(4,2)::f1,f3, f4(2)) ! tcx: base(4,2)
   
   ! formatted I/O operations
   
   write (1, "(A)") "   1.1100   2.2200  33.3000   4.4000  55.5000  66.6000"
   write (1, "(A)") "   2.2200  33.3000  44.4000   5.5000  66.6000  77.7000"
   write (1, "(A)") "   1.1100   2.2200  33.3000   4.4000  55.5000  66.6000   2.2200  33.3000  44.4000   5.5000  66.6000  77.7000"
   
   rewind 1
   
   read (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   read (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 2_4
   
   read (1, *, iostat=stat, iomsg=msg)                f4   
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 3_4
   
   write (6,'(F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X)') f1%r1, f1%c1
   write (6,'(F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X)') f3%r1, f3%c1
   write (6,'(F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X)') f4(1)%r1, f4(1)%c1
   write (6,'(F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X,F8.4,1X)') f4(2)%r1, f4(2)%c1
   
end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   read ( unit, iostat = iostat, fmt = '(E6.2,-1P,1X,D8.2 )')    dtv%r1(1), dtv%r1(2)
   if ( iostat /= 0 ) error stop 10_4
   read ( unit, iostat = iostat, fmt = '(1X,E8.2E1,-2P,1X,E8.2E1,0P,1X,D8.2,1P,1X,D8.2 )')   dtv%c1(1), dtv%c1(2)  
   
   iomsg = 'dtioread'
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 5 changes
