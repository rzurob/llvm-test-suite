! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-21 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.7.6: BN, BZ Editing
!*                                        When BZ mode and  blank is encountered, it should assign value zero
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
      integer, kind :: lb
      integer(kb)   :: i1(lb) = -1 ! (/-1, -1/)
      real(kb)      :: r1(lb) = [-99.9, (-9999.9, i=1,lb-1)]
      complex(kb)   :: c1(lb) = [(-99.9, -9999.9), ((-1.0, 1.0),i=1,lb-1)]
   end type

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base(4,2)), intent(inout) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program blank003kl
   use m1

   ! declaration of variables

   class(base(4,2)), allocatable  :: f1 ! tcx: (4,:)
   type(base(4,2)), pointer       :: f2(:) ! tcx: (4,:)
   type(base(4,2)) , allocatable  :: f3 ! tcx: (4,:)
   class(base(4,2)), pointer      :: f4(:) ! tcx: (4,:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'blank003kl.1', form='formatted', access='sequential', blank='zero' )    !<- initially set blank mode to null

   ! allocation of variables


   allocate (f1,f2(2),f3,f4(2))

   write (1, *)   "!                                                               "
   write (1, *)   "                                                                                                                               "
   write (1, *)   "!                                                               "
   write (1, *)   "                                                                                                                               "

   rewind 1

   ! formatted I/O operations

   read (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 1_4

   read (1, '(2dt)', iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 2_4

   read (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 3_4

   read (1, '(2dt)', iostat=stat, iomsg=msg)                f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 4_4

   write(6, "(I4,1X,I4)")       f1%i1
   write(6, "(2(f8.4,1X))")     f1%r1
   write(6, "(4(f8.4,1X))")     f1%c1

   write(6, "(I4,1X,I4)")       f2(1)%i1
   write(6, "(2(f8.4,1X))")     f2(1)%r1
   write(6, "(4(f8.4,1X))")     f2(1)%c1
   write(6, "(I4,1X,I4)")       f2(2)%i1
   write(6, "(2(f8.4,1X))")     f2(2)%r1
   write(6, "(4(f8.4,1X))")     f2(2)%c1

   write(6, "(I4,1X,I4)")       f3%i1
   write(6, "(2(f8.4,1X))")     f3%r1
   write(6, "(4(f8.4,1X))")     f3%c1

   write(6, "(I4,1X,I4)")       f4(1)%i1
   write(6, "(2(f8.4,1X))")     f4(1)%r1
   write(6, "(4(f8.4,1X))")     f4(1)%c1
   write(6, "(I4,1X,I4)")       f4(2)%i1
   write(6, "(2(f8.4,1X))")     f4(2)%r1
   write(6, "(4(f8.4,1X))")     f4(2)%c1

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base(4,2)), intent(inout) :: dtv ! tcx: (4,*)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read ( unit, iostat = iostat, fmt = '(1X,I4,1X,I4)')          dtv%i1
   if ( iostat /= 0 ) error stop 10_4
10 format (6(1X,F8.4))
   read ( unit, iostat = iostat, fmt = 10 )   dtv%r1, dtv%c1

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,2) / declare with (4,*) - 6 changes
