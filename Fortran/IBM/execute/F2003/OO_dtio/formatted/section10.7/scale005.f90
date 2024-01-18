 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: scale005.f
! %VERIFY: scale005.1:scale005.vf
! %STDIN:
! %STDOUT:
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
!*                                        Inside DTIO, P edit and see if the connection has
!*                                        changed scale factor temporarily for write G editor
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

end module

program scale005
   use m1

   ! declaration of variables

   class(base), allocatable  :: f1
   type(base), pointer       :: f2(:,:)
   type(base) , allocatable  :: f3
   class(base), pointer      :: f4(:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'scale005.1', form='formatted', access='sequential' )

   ! allocation of variables


   ! since the format is G12.4 for output, the range for F editing is 0.099995 < N < 9999.5
   ! any number outside this range, E editing shall be used

   allocate (f1, source = base(  (/0.01,0.099994/), (/ (0.099995, 1.234), (12.34, 123.4) /) ))
   allocate (f2(2,2), source = reshape (source=(/ base( (/1234.0,9999.4/), (/ (9999.5, 12345.0), (0.00001, 0.000012) /) ),  &
                                                  base( (/1.2e-1,.12e-1/), (/ (9900.0, 11.1), (22.2, .000033) /) ),  &
                                                  base( (/9.99,11.1/), (/ (222.00, 33.3), (999.9, 44.4) /) ),  &
                                                  base( (/2.22,33.3/), (/ (.4444, 55.5),  (.006, 70000000) /) )   &
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

end program

subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m1, only: base
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, iostat = iostat, fmt = '(1X,G12.4,-1P,1X,G12.4 )')    dtv%r1(1), dtv%r1(2)
   if ( iostat /= 0 ) error stop 5_4
   write ( unit, iostat = iostat, fmt = '(1X,G12.4,-3P,1X,G12.4,0P,1X,G12.4,1P,1X,G12.4 )')   dtv%c1(1), dtv%c1(2)

   iomsg = 'dtiowrite'

end subroutine
