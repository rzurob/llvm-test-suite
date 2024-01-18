 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scale001a.f
! %VERIFY: scale001a.out:scale001a.vf
! %STDIN:
! %STDOUT: scale001a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.7.5: P Editing
!*                                        Inside DTIO, P edit and see if the connection has changed scale factor temporarily on read
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

   type :: base
      integer(4)   :: i1(2)
      real(4)      :: r1(2)
      complex(4)   :: c1(2)
   end type

   interface write(formatted)
      subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program scale001a
   use m1

   ! declaration of variables

   class(base), allocatable  :: f1
   type(base), pointer       :: f2(:,:)
   type(base) , allocatable  :: f3
   class(base), pointer      :: f4(:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scale001a.1', form='formatted', access='sequential' )

   ! allocation of variables

   allocate (f1, source = base( (/111,222/), (/1.11,2.22/), (/ (33.3, 44.4), (55.5, 66.6) /) ))
   allocate (f2(2,2), source = reshape (source=(/ base( (/333,444/),  (/5.55,6.66/), (/ (77.7, 88.8), (99.9, 11.1) /) ),  &
                                                  base( (/555,666/),  (/7.77,8.88/), (/ (99.9, 11.1), (22.2, 33.3) /) ),  &
                                                  base( (/777,888/),  (/9.99,1.11/), (/ (22.2, 33.3), (99.9, 44.4) /) ),  &
                                                  base( (/999,111/),  (/2.22,3.33/), (/ (44.4, 55.5), (66.6, 77.7) /) )   &
                                                /),shape=(/2,2/)) )
   allocate (f3, source = f2(2,2) )
   allocate (f4(2), source = (/ f1, f3 /) )

   ! formatted I/O operations

   write (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 1_4

   write (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 2_4

   write (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 3_4

   write (1, *, iostat=stat, iomsg=msg)                f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtiowrite' ) )   error stop 4_4

   deallocate ( f1, f2, f3, f4 )

   allocate ( f1, f2(2,2), f3, f4(2))

   rewind 1

   read (1, *, iostat=stat, iomsg=msg)                f1
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 5_4

   read (1, *, iostat=stat, iomsg=msg)                f2
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 6_4

   read (1, *, iostat=stat, iomsg=msg)                f3
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 7_4

   read (1, *, iostat=stat, iomsg=msg)                f4
   if ( ( stat /= 0  ) .or. ( msg /= 'dtioread' ) )   error stop 8_4

   ! check if values are read correctly

   print *, f1
   print *, f2
   print *, f3
   print *, f4

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, iostat = iostat, fmt = '(-1P,I4,1X,I4 )')           dtv%i1(1), dtv%i1(2)
   if ( iostat /= 0 ) error stop 9_4
   write ( unit, iostat = iostat, fmt = '(1X,F8.4,-1P,1X,F8.4 )')   dtv%r1(1), dtv%r1(2)
   if ( iostat /= 0 ) error stop 10_4
   write ( unit, iostat = iostat, fmt = '(1X,F8.4,-3P,1X,F8.4,0P,1X,F8.4,1P,1X,F8.4 )')   dtv%c1(1), dtv%c1(2)

   iomsg = 'dtiowrite'

end subroutine

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read ( unit, iostat = iostat, fmt = '(-1P,I3,1X,I4 )')       dtv%i1(1), dtv%i1(2)       !<- P has no effect on I editor
   if ( iostat /= 0 ) error stop 11_4
   read ( unit, iostat = iostat, fmt = '(1X,F8.4,-1P,1X,F8.4 )')       dtv%r1(1), dtv%r1(2)
   if ( iostat /= 0 ) error stop 12_4
   read ( unit, iostat = iostat, fmt = '(1X,F8.4,-3P,1X,F8.4,0P,1X,F8.4,1P,1X,F8.4 )')   dtv%c1(1), dtv%c1(2)

   iomsg = 'dtioread'

end subroutine
