 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scale002.f
! %VERIFY: scale002.out:scale002.vf
! %STDIN:
! %STDOUT: scale002.out
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
!*                                        Inside DTIO, P editor does not have effect if the input field has exponent
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

program scale002
   use m1

   ! declaration of variables

   class(base), allocatable  :: f1
   type(base) , allocatable  :: f3

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scale002.1', form='formatted', access='sequential' )

   ! allocation of variables

   allocate ( f1, f3)

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
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read ( unit, iostat = iostat, fmt = '(-1P,I3,1X,I4 )')              dtv%i1(1), dtv%i1(2)       !<- P has no effect on I editor
   if ( iostat /= 0 ) error stop 11_4
   read ( unit, iostat = iostat, fmt = '(1X,1P,F8.4,-1P,1X,F8.4 )')       dtv%r1(1), dtv%r1(2)       !<- P has effect on
   if ( iostat /= 0 ) error stop 12_4
   read ( unit, iostat = iostat, fmt = '(2P,1X,F8.4,-3P,1X,F8.4,0P,1X,F8.4,1P,1X,F8.4 )')   dtv%c1(1), dtv%c1(2)

   iomsg = 'dtioread'

end subroutine
