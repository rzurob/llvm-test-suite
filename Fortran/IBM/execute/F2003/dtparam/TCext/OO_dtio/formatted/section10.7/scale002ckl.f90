! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : scale002ckl
!*
!*  PROGRAMMER                 : David Forster (derived from scale002c by Robert Ma)
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
!*                                        Inside DTIO, P editor does not have effect if the 
!*                                        input field has exponent on G editors
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
      integer(kb)   :: i1(lb)
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

program scale002ckl
   use m1   

   ! declaration of variables

   class(base(4,:)), allocatable  :: f1 ! tcx: (4,:)
   type(base(4,:)) , allocatable  :: f3 ! tcx: (4,:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'scale002ckl.1', form='formatted', access='sequential' )
   
   ! allocation of variables
   
   allocate (base(4,2):: f1, f3) ! tcx: base(4,2)
   
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                ' 111  222   1.11e0 222.0e-3  3.330e1  4.44e-2  55.5e-1 666.0e-2'
   write (1, *, iostat=stat, iomsg=msg)                ' 111  222   1.1100   0.2220  33.3000   0.0444  5.55000 6.660000'
      
   rewind 1
   
   read (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   read (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 2_4
      
   ! check if values are read correctly
   
   write (6, "(I4, 1X, I4, 1X, F8.4, 1X, F8.4, 1X, F8.4, 1X, F8.4, 1X, F8.4, 1X, F8.4 )") f1%i1, f1%r1, f1%c1
   write (6, "(I4, 1X, I4, 1X, F8.4, 1X, F8.4, 1X, F8.4, 1X, F8.4, 1X, F8.4, 1X, F8.4 )") f3%i1, f3%r1, f3%c1

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,*)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   read ( unit, iostat = iostat, fmt = '(-1P,I3,1X,I4 )')              dtv%i1(1), dtv%i1(2)       !<- P has no effect on I editor
   if ( iostat /= 0 ) error stop 3_4
   read ( unit, iostat = iostat, fmt = '(1X,1P,G8.4,-1P,1X,G8.4 )')       dtv%r1(1), dtv%r1(2)       !<- P has effect on 
   if ( iostat /= 0 ) error stop 4_4
   read ( unit, iostat = iostat, fmt = '(2P,1X,G8.4,-3P,1X,G8.4,0P,1X,G8.4,1P,1X,G8.4 )')   dtv%c1(1), dtv%c1(2)
   
   iomsg = 'dtioread'
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 4 changes
