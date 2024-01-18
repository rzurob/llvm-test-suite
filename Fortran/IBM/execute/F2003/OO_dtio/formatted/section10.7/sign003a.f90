 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: sign003a.f
! %VERIFY: sign003a.1:sign003a.vf
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

   type :: base
      integer(4)   :: i1(2)
   end type

   type :: base1
      real(4)      :: r1(2)
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

      subroutine writeformattedbase1 (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program sign003a
   use m1

   ! declaration of variables

   class(base), allocatable    :: f1
   class(base), pointer        :: f2(:,:)
   class(base1) , allocatable  :: f3
   class(base1), pointer       :: f4(:)

   integer :: stat
   character(200) :: msg

   open ( 1, file = 'sign003a.1', form='formatted', access='sequential' )

   ! allocation of variables

   allocate (f1, source = base( (/111,222/) ))
   allocate (f2(2,2), source = reshape (source=(/ base( (/333,444/) ),  &
                                                  base( (/555,666/) ),  &
                                                  base( (/777,888/) ),  &
                                                  base( (/999,111/) )   &
                                                /),shape=(/2,2/)) )
   allocate (f3, source = base1( (/1.11,2.22/) ) )
   allocate (f4(2), source = (/ base1( (/33.1,44.2/) ), f3 /) )

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
   class(base), intent(in) :: dtv
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
   class(base1), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write ( unit, iostat = iostat, fmt = '(F8.4,1X,F8.4 )')       dtv%r1(1), dtv%r1(2)       !<- both shall NOT have PLUS signs

   iomsg = 'dtiowrite'

end subroutine
