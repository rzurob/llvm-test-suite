! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : sign003akl
!*
!*  PROGRAMMER                 : David Forster (derived from sign003a by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 10.7.4: SS,SP, and S Editing
!*                                        Inside DTIO, use SS, SP and S editors and see if the connection is changed mode temporarily
!*                                        with input items at parent being different types
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
   end type
   
   type :: base1 (kb1,lb1) ! kb1,lb1=4,2
      integer, kind :: kb1
      integer, len :: lb1
      real(kb1)      :: r1(lb1)
   end type
   
   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
      
      subroutine writeformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine 
   end interface

end module

program sign003akl
   use m1   

   ! declaration of variables

   class(base(4,:)), allocatable    :: f1 ! tcx: (4,:)
   class(base(4,:)), pointer        :: f2(:,:) ! tcx: (4,:)
   class(base1(4,:)) , allocatable  :: f3 ! tcx: (4,:)
   class(base1(4,:)), pointer       :: f4(:) ! tcx: (4,:)
      
   integer :: stat
   character(200) :: msg
   
   open ( 1, file = 'sign003akl.1', form='formatted', access='sequential' )
   
   ! allocation of variables
   
   allocate (f1, source = base(4,2)( (/111,222/) )) ! tcx: (4,2)
   allocate (f2(2,2), source = reshape (source=(/ base(4,2)( (/333,444/) ),  & ! tcx: (4,2)
                                                  base(4,2)( (/555,666/) ),  & ! tcx: (4,2)
                                                  base(4,2)( (/777,888/) ),  & ! tcx: (4,2)
                                                  base(4,2)( (/999,111/) )   & ! tcx: (4,2)
                                                /),shape=(/2,2/)) )
   allocate (f3, source = base1(4,2)( (/1.11,2.22/) ) ) ! tcx: (4,2)
   allocate (f4(2), source = (/ base1(4,2)( (/33.1,44.2/) ), f3 /) ) ! tcx: (4,2)
   
   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1, f3          !<- inside the DTIO call for f1, the sign mode is changed temporarily, should not
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4  !   affect f3
   
   write (1, *, iostat=stat, iomsg=msg)                f1, +5, f3      !<- inside the DTIO call for f1, the sign mode is changed temporarily, should not
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4  !   affect 5 or f3
   
   write (1, *, iostat=stat, iomsg=msg)                f2(1:2,2), f3, f2(1,1:2)    !<- inside the DTIO call for f2, the sign mode is changed temporarily, should not
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4  !   affect f3
   
   write (1, *, iostat=stat, iomsg=msg)                ( f2(i,1), +5, f4(i), i = 1, 2), +2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4  

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write ( unit, iostat = iostat, fmt = '(SP,I4,1X,I4 )')       dtv%i1(1), dtv%i1(2)       !<- both shall have PLUS signs
   
   iomsg = 'dtiowrite'
   
end subroutine

subroutine writeformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base1
   class(base1(4,*)), intent(in) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)   
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
   
   write ( unit, iostat = iostat, fmt = '(F8.4,1X,F8.4 )')       dtv%r1(1), dtv%r1(2)       !<- both shall NOT have PLUS signs
   
   iomsg = 'dtiowrite'
   
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 9 changes
! type: base1 - added parameters (kb1,lb1) to invoke with (4,2) / declare with (4,*) - 6 changes
